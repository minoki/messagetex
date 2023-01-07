{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module InputProcessor where
import           Control.Applicative ((<|>))
import           Control.Monad.Except
import           Control.Monad (guard, unless, when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe
import           Data.Bits (xor)
import qualified Data.ByteString as BS
import           Data.Char (chr, digitToInt, isAlpha, ord)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as T (hexadecimal)
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
isLowerHexDigit c = ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')

nextChar :: Env -> T.Text -> Either String (Maybe (Char, CatCode, T.Text))
nextChar (Env { catcodeMap }) s = runMaybeT $ do
  (c0, rest0) <- hoistMaybe $ T.uncons s
  let cc0 = getCatcode catcodeMap c0
  let superscriptNotation = do guard (cc0 == CCSup)
                               trySuperscriptNotation c0 rest0
  superscriptNotation <|> pure (c0, cc0, rest0)
  where
    trySuperscriptNotation :: Char -> T.Text -> MaybeT (Either String) (Char, CatCode, T.Text)
    trySuperscriptNotation c0 rest0 = do
      (c1, rest1) <- hoistMaybe $ T.uncons rest0
      guard (c0 == c1)
      (c2, rest2) <- hoistMaybe $ T.uncons rest1
      let fourOrSixHexDigits :: MaybeT (Either String) (Char, CatCode, T.Text)
          fourOrSixHexDigits = do guard (c0 == c2)
                                  (c3, rest3) <- hoistMaybe $ T.uncons rest2
                                  guard (c0 == c3)
                                  -- no backtracking
                                  let sixHexDigits = do (c4, rest4) <- hoistMaybe $ T.uncons rest3
                                                        guard (c0 == c4)
                                                        (c5, rest5) <- hoistMaybe $ T.uncons rest4
                                                        guard (c0 == c5)
                                                        let (digits, rest6) = T.splitAt 6 rest5
                                                        when (T.length digits < 6 || not (T.all isLowerHexDigit digits)) $
                                                          throwError "^^^^^^ needs six hex digits"
                                                        (x, rest') <- lift $ T.hexadecimal digits
                                                        unless (T.null rest') $ throwError "^^^^^^ needs six hex digits" -- should not occur
                                                        unless (isUnicodeScalarValue x) $ throwError "invalid Unicode scalar value"
                                                        let c = chr x
                                                        pure (c, getCatcode catcodeMap c, rest6)
                                  let fourHexDigits = do let (digits, rest4) = T.splitAt 4 rest3
                                                         when (T.length digits < 4 || not (T.all isLowerHexDigit digits)) $
                                                           throwError "^^^^ needs four hex digits"
                                                         (x, rest') <- T.hexadecimal digits
                                                         unless (T.null rest') $ throwError "^^^^ needs four hex digits" -- should not occur
                                                         unless (isUnicodeScalarValue x) $ throwError "invalid Unicode scalar value"
                                                         let c = chr x
                                                         pure (c, getCatcode catcodeMap c, rest4)
                                  sixHexDigits <|> lift fourHexDigits
      let twoHexDigits :: Maybe (Char, CatCode, T.Text)
          twoHexDigits = do guard (isLowerHexDigit c2)
                            (c3, rest3) <- T.uncons rest2
                            guard (isLowerHexDigit c3)
                            let c = chr (digitToInt c2 * 16 + digitToInt c3)
                            pure (c, getCatcode catcodeMap c, rest3)
      let xor64 = let c = chr (ord c2 `xor` 64)
                  in (c, getCatcode catcodeMap c, rest2)
      result <- fourOrSixHexDigits <|> hoistMaybe twoHexDigits <|> pure xor64
      case result of
        (c, CCSup, rest) -> trySuperscriptNotation c rest <|> pure result
        _                -> pure result

nextToken :: Env -> State -> Either String (Maybe (Token, State))
nextToken _env (State { lines = [], lineState = _ }) = pure Nothing
nextToken env@(Env { catcodeMap = _, endlinechar }) (State { lines = currentLine : rest, lineState })
  = do let nextStateWithNewLine = State { lines = case rest of
                                                    l : ls -> appendEndlinechar l endlinechar : ls
                                                    [] -> []
                                        , lineState = NewLine
                                        }
       n <- nextChar env currentLine
       case n of
         Nothing -> nextToken env nextStateWithNewLine
         Just (c, cc, restOfLine) ->
           let nextState = State { lines = restOfLine : rest, lineState = MiddleOfLine }
           in case cc of
             CCEscape -> do n' <- nextChar env restOfLine
                            case n' of
                              Nothing -> pure $ Just (TCommandName (ControlSeq ""), State { lines = T.empty : rest, lineState = SkipSpaces }) -- Since the control sequence may change \endlinechar, don't go to next line here
                              Just (c1, CCLetter, restOfLine1) -> -- control word
                                let go l acc = do n'' <- nextChar env l
                                                  case n'' of
                                                    Nothing -> pure $ Just (TCommandName (ControlSeq (T.pack (reverse acc))), State { lines = T.empty : rest, lineState = SkipSpaces })
                                                    Just (c', CCLetter, restOfLine') -> go restOfLine' (c' : acc)
                                                    Just (c', _, restOfLine') -> pure $ Just (TCommandName (ControlSeq (T.pack (reverse acc))), State { lines = T.cons c' restOfLine' : rest, lineState = SkipSpaces })
                                in go restOfLine1 [c1]
                              Just (c1, cc1, restOfLine1) -> -- control symbol / space
                                pure $ Just (TCommandName (ControlSeq (T.singleton c1)), State { lines = restOfLine1 : rest, lineState = if cc1 == CCSpace then SkipSpaces else MiddleOfLine })
             CCBeginGroup -> pure $ Just (TCharacter c CCBeginGroup, nextState)
             CCEndGroup -> pure $ Just (TCharacter c CCEndGroup, nextState)
             CCMathShift -> pure $ Just (TCharacter c CCMathShift, nextState)
             CCAlignmentTab -> pure $ Just (TCharacter c CCAlignmentTab, nextState)
             CCEndLine -> case lineState of
               NewLine      -> pure $ Just (parToken, nextStateWithNewLine)
               MiddleOfLine -> pure $ Just (spaceToken, nextStateWithNewLine)
               SkipSpaces   -> nextToken env nextStateWithNewLine -- ignored
             CCParam -> pure $ Just (TCharacter c CCParam, nextState)
             CCSup -> pure $ Just (TCharacter c CCSup, nextState)
             CCSub -> pure $ Just (TCharacter c CCSub, nextState)
             CCIgnored -> nextToken env (State { lines = restOfLine : rest, lineState = lineState }) -- keep line state
             CCSpace -> case lineState of
               MiddleOfLine -> pure $ Just (spaceToken, State { lines = restOfLine : rest, lineState = SkipSpaces })
               _ -> nextToken env (State { lines = restOfLine : rest, lineState = lineState })
             CCLetter -> pure $ Just (TCharacter c CCLetter, nextState)
             CCOther -> pure $ Just (TCharacter c CCOther, nextState)
             CCActive -> pure $ Just (TCommandName (ActiveChar c), nextState)
             CCComment -> nextToken env nextStateWithNewLine
             CCInvalid -> throwError "invalid character" -- nextToken env (State { lines = restOfLine : rest, lineState = lineState })
