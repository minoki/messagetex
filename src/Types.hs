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
                 deriving (Eq, Show)

data Token = TCommandName !CommandName
           | TCharacter !Char !CatCode -- one of CCBeginGroup, CCEndGroup, CCMathShift, CCAlignmentTab, CCParam, CCSup, CCSub, CCSpace, CCLetter, CCOther
           deriving Show
           -- TODO: 'noexpand' flag?
           -- TODO: parameter token?
