{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module ExpansionProcessor where
import           Control.Monad (when)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict hiding (State)
import           Data.Int (Int32)
import qualified Data.IntMap.Strict as IntMap
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified InputProcessor as IP
import           Types

data EToken = EToken { depth :: !Int, token :: !Token }
            deriving Show

data Env = Env

data LocalState = LocalState { scopeType     :: !ScopeType
                             , controlSeqMap :: Map.Map T.Text Value
                             , activeCharMap :: Map.Map Char Value -- use IntMap?
                             , inputEnv      :: !IP.Env -- catcodeMap and endlinechar
                             -- lccodeMap, uccodeMap, mathcodeMap, delcodeMap, sfcodeMap
                             , escapechar    :: !Int
                             , countReg      :: IntMap.IntMap Int32
                             -- dimenReg, skipReg, muskipReg, toksReg, box registers
                             -- thinmuskip, medmuskip, thickmuskip
                             }

data ConditionalKind = CondTruthy
                     | CondFalsy
                     | CondCase
                     | CondTest

data State = State { inputState       :: !IP.State
                   , pendingTokens    :: [EToken]
                   , localStates      :: NE.NonEmpty LocalState
                   , conditionalStack :: [ConditionalKind]
                   }

data Context = Context { maxExpansionDepth :: !Int
                       , maxPendingToken   :: !Int
                       }

type M = ReaderT Context (StateT State (Either String))

enter :: ScopeType -> M ()
enter !st = modify $ \s@(State { localStates = localStates@(ls :| _) }) ->
                       s { localStates = ls { scopeType = st } NE.<| localStates }

leave :: ScopeType -> M ()
leave !st = do
  LocalState { scopeType } :| lss <- gets localStates
  if scopeType == st
    then case lss of
           []      -> throwError ("Extra " ++ ending st) -- or "Too many }'s"
           ls:lss' -> modify $ \s -> s { localStates = ls :| lss' }
    else throwError $ "mismatched braces: begun by " ++ beginning st ++ ", ended by " ++ ending st
  where
    beginning ScopeByBrace      = "left brace `{'"
    beginning ScopeByBeginGroup = "\\begingroup"
    beginning GlobalScope       = "<beginning of input>"
    beginning ScopeByLeftRight  = "\\left"
    beginning ScopeByMathShift  = "`$' or `$$'"
    ending ScopeByBrace      = "right brace `}'"
    ending ScopeByBeginGroup = "\\endgroup"
    ending GlobalScope       = "<end of input>"
    ending ScopeByLeftRight  = "\\right"
    ending ScopeByMathShift  = "`$' or `$$'"

getLocalState :: M LocalState
getLocalState = gets (NE.head . localStates)

lookupCommand :: CommandName -> M Value
lookupCommand (ControlSeq name) = do LocalState { controlSeqMap } <- getLocalState
                                     pure $ case Map.lookup name controlSeqMap of
                                              Nothing -> Expandable Eundefined
                                              Just v  -> v
lookupCommand (ActiveChar c) = do LocalState { activeCharMap } <- getLocalState
                                  pure $ case Map.lookup c activeCharMap of
                                           Nothing -> Expandable Eundefined
                                           Just v  -> v
lookupCommand FrozenRelax = pure $ Nonexpandable $ Nrelax { noexpand = False }

nextRawToken :: M (Maybe EToken)
nextRawToken = do p <- gets pendingTokens
                  case p of
                    [] -> do ls <- getLocalState
                             is <- gets inputState
                             m <- lift $ lift $ IP.nextToken (inputEnv ls) is
                             case m of
                               Nothing -> pure Nothing
                               Just (t, is') -> do
                                 modify (\s -> s { inputState = is' })
                                 pure $ Just $ EToken { depth = 0, token = t }
                    t:ts -> do modify (\s -> s { pendingTokens = ts })
                               pure $ Just t

nextTokenWithoutExpansion :: M (Maybe Token)
nextTokenWithoutExpansion = fmap token <$> nextRawToken

unreadTokens :: Int -> [Token] -> M ()
unreadTokens !depth tokens = do
  Context { maxExpansionDepth = maxDepth, maxPendingToken = maxLen } <- ask
  when (depth >= maxDepth) $ throwError "recursion too deep"
  tokens' <- gets pendingTokens
  when (length tokens + length tokens' > maxLen) $ throwError "token list too long"
  modify $ \s -> s { pendingTokens = map (\t -> EToken { depth = depth, token = t }) tokens ++ tokens' }

unreadToken :: EToken -> M ()
unreadToken token@(EToken { depth }) = do
  Context { maxExpansionDepth = maxDepth, maxPendingToken = maxLen } <- ask
  when (depth >= maxDepth) $ throwError "recursion too deep"
  tokens' <- gets pendingTokens
  when (1 + length tokens' > maxLen) $ throwError "token list too long"
  modify $ \s -> s { pendingTokens = token : tokens' }

nextExpandedToken :: M (Maybe (Token, Nonexpandable))
nextExpandedToken = do
  r <- nextRawToken
  case r of
    Nothing -> pure Nothing
    Just (EToken { depth, token = t@(TCommandName { name, noexpand = False }) }) -> do
      m <- lookupCommand name
      case m of
        Expandable e -> do
          p <- expand e t
          unreadTokens (depth + 1) p
          nextExpandedToken
        Nonexpandable n -> pure $ Just (t, n)
    Just (EToken { token = t@(TCommandName { noexpand = True }) }) -> do
      pure $ Just (t, Nrelax { noexpand = True })
    Just (EToken { token = t@(TCharacter c cc) }) ->
      pure $ Just (t, Character c cc)

mor :: Monad m => m (Maybe a) -> m a -> m a
mor action e = do r <- action
                  case r of
                    Nothing -> e
                    Just x  -> pure x

readUntilEndcsname :: M T.Text
readUntilEndcsname = loop []
  where
    loop acc = do
      m <- nextExpandedToken `mor` throwError "unexpected end of input while looking for \\endcsname" -- Missing \endcsname inserted
      case m of
        (TCharacter c _, _) -> loop (c : acc)
        (_, Nendcsname) -> pure $ T.pack $ reverse acc
        _ -> throwError "unexpected nonexpandable token while looking for \\endcsname" -- Missing \endcsname inserted

expand :: Expandable -> Token -> M [Token]
expand Eundefined _ = throwError "Undefined control sequence"
expand Ecsname _ = do
  name <- readUntilEndcsname
  let cname = ControlSeq name
  d <- lookupCommand cname
  case d of
    Expandable Eundefined ->
      -- THE DREADED SIDE EFFECT OF \csname
      modify $ \s@(State { localStates = l@(LocalState { controlSeqMap }) NE.:| ls }) ->
                 let l' = l { controlSeqMap = Map.insert name (Nonexpandable $ Nrelax { noexpand = False }) controlSeqMap }
                 in s { localStates = l' NE.:| ls }
    _ -> pure ()
  pure [TCommandName { name = cname, noexpand = False }]
expand Enoexpand _ = do
  t <- nextTokenWithoutExpansion `mor` throwError "\\noexpand"
  case t of
    TCommandName { name, noexpand = False } -> do
      v <- lookupCommand name
      pure $ case v of
        Expandable _    -> [TCommandName { name = name, noexpand = True }]
        Nonexpandable _ -> [t]
    _ -> pure [t]
expand Eexpandafter _ = do
  t1 <- nextTokenWithoutExpansion `mor` throwError "\\expandafter"
  t2 <- nextTokenWithoutExpansion `mor` throwError "\\expandafter"
  expanded <- case t2 of
                TCommandName { name, noexpand = True } -> pure [TCommandName { name = name, noexpand = False }]
                TCommandName { name, noexpand = False } -> do
                  m <- lookupCommand name
                  case m of
                    Expandable e    -> expand e t2
                    Nonexpandable _ -> pure [t2]
                _ -> pure [t2] -- character
  pure $ t1 : expanded

expandInEdef :: Expandable -> Token -> M (Maybe [Token])
expandInEdef _ _ = pure Nothing

readFillerAndLBrace :: Bool -> M ()
readFillerAndLBrace !createScope = loop
  where
    loop :: M ()
    loop = do
      (t, v) <- nextExpandedToken `mor` throwError "reached EOF while looking for <general text>"
      case v of
        Character _ CCBeginGroup | createScope -> enter ScopeByBrace -- explicit or implicit left brace
                                 | otherwise -> pure ()
        Nrelax {} -> loop -- \relax: ignored
        Character _ CCSpace -> loop -- optional spaces: ignored
        _ -> throwError $ "got " ++ show t ++ " while looking for <general text>" -- Missing { inserted

edefReadUntilEndGroup :: M [Token]
edefReadUntilEndGroup = loop 0 []
  where
    loop :: Int -> [Token] -> M [Token]
    loop !depth revTokens = do
      EToken { depth = d, token = t } <- nextRawToken `mor` throwError "expected '}', but got EOF"
      case t of
        TCharacter _ CCEndGroup | depth == 0 -> pure $ reverse revTokens
                                | otherwise -> loop (depth - 1) (t : revTokens)
        TCharacter _ CCBeginGroup -> loop (depth + 1) (t : revTokens)
        TCommandName { name, noexpand = False } -> do
          m <- lookupCommand name
          case m of
            Expandable e -> do
              u <- expandInEdef e t
              case u of
                Nothing -> do
                  p <- expand e t
                  unreadTokens (d + 1) p
                  loop depth revTokens
                Just p -> loop depth (reverse p ++ revTokens)
            Nonexpandable _ -> loop depth (t : revTokens)
        -- character, \noexpand-ed name
        _ -> loop depth (t : revTokens) -- noexpand flag should be stripped later

readExpandedGeneralText :: M [Token]
readExpandedGeneralText = do
  readFillerAndLBrace True
  content <- edefReadUntilEndGroup
  leave ScopeByBrace
  pure content
