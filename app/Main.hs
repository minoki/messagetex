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
import           System.Exit (exitFailure)
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

type M = ReaderT XP.Context (StateT XP.State IO)

runM :: XP.M a -> M a
runM action = do ctx <- ask
                 s <- get
                 case runStateT (runReaderT action ctx) s of
                   Left e -> liftIO $ do hPutStrLn stderr e
                                         exitFailure
                   Right (result, s') -> do put s'
                                            pure result

mainLoop :: M ()
mainLoop = do r <- runM XP.nextExpandedToken
              case r of
                Nothing -> pure () -- no more token
                Just (_t, n) -> case n of
                  Character c cc -> do liftIO $ putStrLn $ "Character " ++ show c ++ " " ++ show cc
                                       mainLoop
                  DefinedCharacter c -> do liftIO $ putStrLn $ "DefinedCharacter " ++ show c
                                           mainLoop
                  Nrelax {} -> mainLoop
                  Nendcsname -> liftIO $ do hPutStrLn stderr "Extra \\endcsname"
                                            exitFailure
                  Nlet -> liftIO $ do hPutStrLn stderr "\\let: not implemented yet"
                                      exitFailure
                  Nmessage -> do tokens <- runM XP.readExpandedGeneralText
                                 s <- get
                                 liftIO $ putStrLn $ concatMap (tokenToString s) tokens
                                 mainLoop

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
  evalStateT (runReaderT mainLoop ctx) initialState
