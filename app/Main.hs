{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.Reader
import           Control.Monad.State.Strict hiding (State)
import           Data.Char (chr, ord)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T (getContents)
import qualified ExpansionProcessor as XP
import qualified InputProcessor as IP
import           Prelude hiding (lines)
import           System.IO (hPutStrLn, stderr)
import           Types

csToString :: Bool -> XP.State -> T.Text -> String
csToString appendSpace s name = case T.uncons name of
  Nothing -> csToString False s "csname" <> csToString appendSpace s "endcsname"
  Just (c, rest) ->
    let ls = NE.head (XP.localStates s)
        escapechar = XP.escapechar ls
        catcodeMap = IP.catcodeMap (XP.inputEnv ls)
        escapechar' = if IP.isUnicodeScalarValue escapechar then
                        [chr escapechar]
                      else
                        []
        appendSpace' = appendSpace && (not (T.null rest) || IP.getCatcode catcodeMap c == CCLetter)
    in escapechar' ++ T.unpack name ++ (if appendSpace' then " " else "")

tokenToString :: XP.State -> Token -> String
tokenToString _ (TCharacter c _) = [c]
tokenToString _ (TCommandName { name = ActiveChar c }) = [c]
tokenToString s (TCommandName { name = ControlSeq name }) = csToString True s name
tokenToString s (TCommandName { name = FrozenRelax }) = csToString True s "relax"

run :: XP.Context -> XP.State -> IO ()
run ctx s = case runStateT (runReaderT XP.nextExpandedToken ctx) s of
              Left e -> hPutStrLn stderr e
              Right (Nothing, _s) -> pure ()
              Right (Just (_t, n), s') -> case n of
                Character c cc -> do putStrLn $ "Character " ++ show c ++ " " ++ show cc
                                     run ctx s'
                DefinedCharacter c -> do putStrLn $ "DefinedCharacter " ++ show c
                                         run ctx s'
                Nrelax {} -> run ctx s'
                Nendcsname -> hPutStrLn stderr "Extra \\endcsname"
                Nlet -> hPutStrLn stderr "\\let: not implemented yet"
                Nmessage -> case runStateT (runReaderT XP.readExpandedGeneralText ctx) s' of
                  Left e -> hPutStrLn stderr e
                  Right (tokens, s'') -> do putStrLn $ concatMap (tokenToString s'') tokens
                                            run ctx s''

main :: IO ()
main = do
  c <- T.getContents
  let env = IP.initialEnv
      initialInputState = IP.newState env $ map (T.dropWhileEnd (== ' ')) $ T.lines c
      initialState = XP.State { XP.inputState = initialInputState
                              , XP.pendingTokens = []
                              , XP.localStates = NE.singleton $ XP.LocalState { XP.scopeType = GlobalScope
                                                                              , XP.controlSeqMap = Map.fromList [("csname", Expandable Ecsname)
                                                                                                                ,("noexpand", Expandable Enoexpand)
                                                                                                                ,("expandafter", Expandable Eexpandafter)
                                                                                                                ,("relax", Nonexpandable (Nrelax False))
                                                                                                                ,("endcsname", Nonexpandable Nendcsname)
                                                                                                                ,("let", Nonexpandable Nlet)
                                                                                                                ,("message", Nonexpandable Nmessage)
                                                                                                                ]
                                                                              , XP.activeCharMap = Map.empty
                                                                              , XP.inputEnv = env
                                                                              , XP.escapechar = ord '\\'
                                                                              , XP.countReg = IntMap.empty
                                                                              }
                              , XP.conditionalStack = []
                              }
      ctx = XP.Context { XP.maxExpansionDepth = 100, XP.maxPendingToken = 100 }
  run ctx initialState
