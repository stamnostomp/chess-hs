module Main where

import qualified Chess.UI.Terminal as Terminal

-- | Main entry point - terminal only version
main :: IO ()
main = do
  putStrLn "Chess Terminal Interface"
  putStrLn "------------------------"
  Terminal.run
