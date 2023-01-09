{-# LANGUAGE NamedFieldPuns #-}
module Primitives where
import           Control.Monad.Except
import           Control.Monad.State
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import           ExpansionProcessor
import           Types

letCommand :: M ()
letCommand = do name <- readCommandName
                readUnexpandedEquals
                readUnexpandedOneOptionalSpace
                t <- nextTokenWithoutExpansion `mor` throwError "\\let"
                v <- meaningWithoutExpansion t
                case name of
                  ControlSeq name' ->
                    modify $ \s@(State { localStates = l@(LocalState { controlSeqMap }) :| ls }) ->
                               let l' = l { controlSeqMap = Map.insert name' v controlSeqMap }
                               in s { localStates = l' :| ls }
                  ActiveChar c ->
                    modify $ \s@(State { localStates = l@(LocalState { activeCharMap }) :| ls }) ->
                               let l' = l { activeCharMap = Map.insert c v activeCharMap }
                               in s { localStates = l' :| ls }
                  FrozenRelax -> throwError "attempt to override frozen \\relax"
