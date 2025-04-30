{-# LANGUAGE CPP #-}
module Main where

import Options.Applicative
import qualified Chess.UI.Terminal as Terminal

#ifdef ENABLE_GTK
import qualified Chess.UI.GTK as GTK
#endif

-- | Command line options
data Options = Options
  { useGtk :: Bool
  }

-- | Command line parser
optionsParser :: Parser Options
optionsParser = Options
  <$> switch
      ( long "gtk"
      <> help "Use GTK graphical interface" )

-- | Parse command line arguments
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Chess game in Haskell"
      <> header "chess-hs - a chess game with terminal and GTK interfaces" )

-- | Main entry point
main :: IO ()
main = do
  options <- parseOptions

  if useGtk options
    then do
#ifdef ENABLE_GTK
      putStrLn "Starting GTK interface..."
      GTK.run
#else
      putStrLn "GTK interface not available. Starting terminal interface..."
      Terminal.run
#endif
    else do
      putStrLn "Starting terminal interface..."
      Terminal.run
