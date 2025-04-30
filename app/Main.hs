module Main where

import Options.Applicative
import qualified Chess.UI.Terminal as Terminal
import qualified Chess.UI.GTK as GTK

data UIMode = Terminal | GTK

uiOption :: Parser UIMode
uiOption = flag Terminal GTK
  ( long "gtk"
  <> short 'g'
  <> help "Use GTK interface instead of terminal" )

opts :: ParserInfo UIMode
opts = info (uiOption <**> helper)
  ( fullDesc
  <> progDesc "Play chess in terminal or GTK interface"
  <> header "chess-hs - a chess game written in Haskell" )

main :: IO ()
main = do
  mode <- execParser opts
  case mode of
    Terminal -> Terminal.run
    GTK -> GTK.run
