{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Types where
import qualified Data.Text as T

data CatCode = CCEscape -- 0
             | CCBeginGroup -- 1
             | CCEndGroup -- 2
             | CCMathShift -- 3
             | CCAlignmentTab -- 4
             | CCEndLine -- 5
             | CCParam -- 6
             | CCSup -- 7
             | CCSub -- 8
             | CCIgnored -- 9
             | CCSpace -- 10
             | CCLetter -- 11
             | CCOther -- 12
             | CCActive -- 13
             | CCComment -- 14
             | CCInvalid -- 15
             deriving (Eq, Show, Enum, Bounded)

data CommandName = ControlSeq !T.Text
                 | ActiveChar !Char
                 | FrozenRelax
                 deriving (Eq, Ord, Show)

data Token = TCommandName { name :: !CommandName
                          , noexpand :: !Bool
                          }
           | TCharacter !Char !CatCode -- one of CCBeginGroup, CCEndGroup, CCMathShift, CCAlignmentTab, CCParam, CCSup, CCSub, CCSpace, CCLetter, CCOther
           deriving Show
           -- TODO: parameter token?

data Expandable = Eundefined
                | Ecsname
                | Enoexpand
                | Eexpandafter
                deriving (Eq, Show)

data Nonexpandable = Character !Char !CatCode
                   | DefinedCharacter !Char -- defined by \chardef
                   | Nrelax { noexpand :: !Bool }
                   | Nendcsname
                   | Nlet
                   | Nmessage
                   deriving (Eq, Show)

data Value = Expandable !Expandable
           | Nonexpandable !Nonexpandable
           deriving (Eq, Show)

data ScopeType = ScopeByBrace -- { .. }
               | ScopeByBeginGroup -- \begingroup .. \endgroup
               | GlobalScope -- \input
               | ScopeByLeftRight -- \left .. \right
               | ScopeByMathShift -- $ .. $ or $$ .. $$
               deriving (Eq, Show)
