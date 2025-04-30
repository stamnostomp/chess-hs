module Main where

import Data.GI.Base (nullToNothing)

main :: IO ()
main = do
  putStrLn "Hello, Haskell Chess (Testing GI Base)!"

  -- Test that the GI base library is working
  let result = nullToNothing (Nothing :: Maybe Int)
  putStrLn $ "GI Base test: " ++ show result
