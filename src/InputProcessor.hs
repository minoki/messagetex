{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module InputProcessor where
import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Bits (xor)
import qualified Data.ByteString as BS
import           Data.Char (chr, digitToInt, isAlpha, ord)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Prelude hiding (lines)
import           Types

isUnicodeScalarValue :: Int -> Bool
isUnicodeScalarValue x = 0 <= x && x <= 0x10FFFF && not (0xD800 <= x && x <= 0xDFFF)

data Env = Env { catcodeMap  :: !(Map.Map Char CatCode)
               , endlinechar :: !Int
               }
           deriving Show

defaultCatcodeMap :: Map.Map Char CatCode
defaultCatcodeMap = Map.fromList [('\\', CCEscape) -- IniTeX
                                 ,('{', CCBeginGroup)
                                 ,('}', CCEndGroup)
                                 ,('$', CCMathShift)
                                 ,('&', CCAlignmentTab)
                                 ,('\r', CCEndLine)
                                 ,('#', CCParam)
                                 ,('^', CCSup)
                                 ,('_', CCSub)
                                 ,('\NUL', CCIgnored)
                                 ,(' ', CCSpace)
                                 ,('~', CCActive)
                                 ,('%', CCComment) -- IniTeX
                                 ,('\DEL', CCInvalid) -- IniTeX
                                 ]

initialEnv :: Env
initialEnv = Env { catcodeMap = defaultCatcodeMap
                 , endlinechar = 13 -- '\r'
                 }

data LineState = NewLine -- State 'N'
               | SkipSpaces -- State 'S'
               | MiddleOfLine -- State 'M'
               deriving (Eq, Show)

data State = State { lines     :: [T.Text]
                   , lineState :: !LineState
                   }
             deriving Show

readFileAsLines :: FilePath -> IO [T.Text]
readFileAsLines path = do content <- BS.readFile path
                          let content' = TE.decodeUtf8 content
                          return $ map (T.dropWhileEnd (== ' ')) $ T.lines content' -- strip whitespace at line ending (The TeXbook, Chapter 8, page 46)

appendEndlinechar :: T.Text -> Int -> T.Text
appendEndlinechar t i | isUnicodeScalarValue i = T.snoc t (chr i)
                      | otherwise = t

newState :: Env -> [T.Text] -> State
newState (Env { endlinechar }) lines = State { lines = case lines of
                                                         l : ls -> appendEndlinechar l endlinechar : ls
                                                         [] -> []
                                             , lineState = NewLine
                                             }

getCatcode :: Map.Map Char CatCode -> Char -> CatCode
getCatcode m c = case Map.lookup c m of
                   Just cc -> cc
                   Nothing -> if isAlpha c then
                                CCLetter
                              else
                                CCOther

parToken :: Token
parToken = TCommandName (ControlSeq "par")

spaceToken :: Token
spaceToken = TCharacter ' ' CCSpace

isLowerHexDigit :: Char -> Bool
isLowerHexDigit c = ('0' <= c && c <= '9') && ('a' <= c && c <= 'f')

nextChar :: Env -> T.Text -> Maybe (Char, CatCode, T.Text)
nextChar (Env { catcodeMap }) s = do
  (c0, rest0) <- T.uncons s
  let cc0 = getCatcode catcodeMap c0
  let doubleSuperscript = do guard (cc0 == CCSup)
                             tryDoubleSuperScript c0 rest0
  doubleSuperscript <|> pure (c0, cc0, rest0)
  where
    tryDoubleSuperScript c0 rest0 = do
      (c1, rest1) <- T.uncons rest0
      guard (c0 == c1)
      (c2, rest2) <- T.uncons rest1
      let twoHexDigits = do guard (isLowerHexDigit c2)
                            (c3, rest3) <- T.uncons rest2
                            guard (isLowerHexDigit c3)
                            let c = chr (digitToInt c2 * 16 + digitToInt c3)
                            pure (c, getCatcode catcodeMap c, rest3)
      -- TODO: quad superscript, six superscript (no backtracking)
      let xor64 = do let c = chr (ord c2 `xor` 64)
                     pure (c, getCatcode catcodeMap c, rest2)
      result <- twoHexDigits <|> xor64
      case result of
        (c, CCSup, rest) -> tryDoubleSuperScript c rest <|> pure result
        _                -> pure result

nextToken :: Env -> State -> Maybe (Token, State)
nextToken _env (State { lines = [], lineState = _ }) = Nothing
nextToken env@(Env { catcodeMap = _, endlinechar }) (State { lines = currentLine : rest, lineState })
  = let nextStateWithNewLine = State { lines = case rest of
                                                 l : ls -> appendEndlinechar l endlinechar : ls
                                                 [] -> []
                                     , lineState = NewLine
                                     }
    in case nextChar env currentLine of
         Nothing -> nextToken env nextStateWithNewLine
         Just (c, cc, restOfLine) ->
           let nextState = State { lines = restOfLine : rest, lineState = MiddleOfLine }
           in case cc of
             CCEscape -> case nextChar env restOfLine of
                           Nothing -> Just (TCommandName (ControlSeq ""), State { lines = T.empty : rest, lineState = SkipSpaces }) -- Since the control sequence may change \endlinechar, don't go to next line here
                           Just (c1, CCLetter, restOfLine1) -> -- control word
                             let go l acc = case nextChar env l of
                                              Nothing -> Just (TCommandName (ControlSeq (T.pack (reverse acc))), State { lines = T.empty : rest, lineState = SkipSpaces })
                                              Just (c', CCLetter, restOfLine') -> go restOfLine' (c' : acc)
                                              Just (c', _, restOfLine') -> Just (TCommandName (ControlSeq (T.pack (reverse acc))), State { lines = T.cons c' restOfLine' : rest, lineState = SkipSpaces })
                             in go restOfLine1 [c1]
                           Just (c1, cc1, restOfLine1) -> -- control symbol / space
                             Just (TCommandName (ControlSeq (T.singleton c1)), State { lines = restOfLine1 : rest, lineState = if cc1 == CCSpace then SkipSpaces else MiddleOfLine })
             CCBeginGroup -> Just (TCharacter c CCBeginGroup, nextState)
             CCEndGroup -> Just (TCharacter c CCEndGroup, nextState)
             CCMathShift -> Just (TCharacter c CCMathShift, nextState)
             CCAlignmentTab -> Just (TCharacter c CCAlignmentTab, nextState)
             CCEndLine -> case lineState of
               NewLine      -> Just (parToken, nextStateWithNewLine)
               MiddleOfLine -> Just (spaceToken, nextStateWithNewLine)
               SkipSpaces   -> nextToken env nextStateWithNewLine -- ignored
             CCParam -> Just (TCharacter c CCParam, nextState)
             CCSup -> Just (TCharacter c CCSup, nextState)
             CCSub -> Just (TCharacter c CCSub, nextState)
             CCIgnored -> nextToken env (State { lines = restOfLine : rest, lineState = lineState }) -- keep line state
             CCSpace -> case lineState of
               MiddleOfLine -> Just (spaceToken, State { lines = restOfLine : rest, lineState = SkipSpaces })
               _ -> nextToken env (State { lines = restOfLine : rest, lineState = lineState })
             CCLetter -> Just (TCharacter c CCLetter, nextState)
             CCOther -> Just (TCharacter c CCOther, nextState)
             CCActive -> Just (TCommandName (ActiveChar c), nextState)
             CCComment -> nextToken env nextStateWithNewLine
             CCInvalid -> nextToken env (State { lines = restOfLine : rest, lineState = lineState }) -- TODO: raise error
