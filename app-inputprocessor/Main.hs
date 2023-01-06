module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T (getContents)
import qualified InputProcessor as IP
import           Prelude hiding (lines)

main :: IO ()
main = do
  c <- T.getContents
  let env = IP.initialEnv
      initialState = IP.newState env $ map (T.dropWhileEnd (== ' ')) $ T.lines c
      go state = case IP.nextToken env state of
                   Nothing -> pure ()
                   Just (t, state') -> print t >> go state'
  go initialState
