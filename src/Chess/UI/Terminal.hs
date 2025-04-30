module Chess.UI.Terminal (run) where

import Control.Monad (when)
import System.Console.ANSI hiding (White, Black) -- Hide ANSI colors to avoid conflict
import qualified System.Console.ANSI as ANSI
import System.IO
import Data.Char (toLower, isDigit)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map as Map

import Chess.Board
import Chess.Game
import Chess.GameState
import Chess.Move
import Chess.Pieces -- This defines our White and Black for piece colors
import Chess.Rules (isLegalMove)

-- | Run the terminal UI
run :: IO ()
run = do
  hSetBuffering stdout NoBuffering
  clearScreen
  setCursorPosition 0 0

  putStrLn "Chess Terminal Interface"
  putStrLn "------------------------"

  gameLoop initialGameState

-- | Main game loop
gameLoop :: GameState -> IO ()
gameLoop game = do
  -- Display the game state
  displayGame game

  -- Check if game is over
  if isGameOver game
    then do
      putStrLn $ case gameStatus game of
        Checkmate -> "Checkmate! " ++ show (fromMaybe (opponent (currentPlayer game)) (getWinner game)) ++ " wins!"
        Stalemate -> "Stalemate! The game is a draw."
        _ -> "Game over."
    else do
      -- Get the player's move
      putStr $ "\n" ++ show (currentPlayer game) ++ "'s move: "
      input <- getLine

      case parseMove input of
        Just move -> do
          if isLegalMove move game
            then gameLoop (makeMove move game)
            else do
              putStrLn "Illegal move. Try again."
              gameLoop game
        Nothing -> do
          putStrLn "Invalid move format. Use 'e2e4' format."
          gameLoop game

-- | Display the current game state
displayGame :: GameState -> IO ()
displayGame game = do
  clearScreen
  setCursorPosition 0 0

  putStrLn $ "Turn: " ++ show (currentPlayer game) ++
             case gameStatus game of
               InProgress -> ""
               Check -> " (Check)"
               _ -> " (Game over)"

  putStrLn ""
  putStrLn $ showBoardWithColors (gameBoard game)

  -- Show move history
  when (not (null (gameMoves game))) $ do
    putStrLn "\nMove history:"
    mapM_ (putStr . (++ " ")) (map show (reverse (gameMoves game)))
    putStrLn ""

-- | Parse a move from string input (e.g., "e2e4")
parseMove :: String -> Maybe Move
parseMove [src1, src2, dst1, dst2]
  | isValidChar src1 && isValidChar dst1 && isValidNum src2 && isValidNum dst2 =
      let srcFile = charToFile src1
          srcRank = charToRank src2
          dstFile = charToFile dst1
          dstRank = charToRank dst2
      in if isValidPosition (srcFile, srcRank) && isValidPosition (dstFile, dstRank)
         then Just (Move (srcFile, srcRank) (dstFile, dstRank) Normal)
         else Nothing
  where
    isValidChar c = c `elem` "abcdefgh"
    isValidNum c = c `elem` "12345678"
    charToFile c = fromEnum (toLower c) - fromEnum 'a'
    charToRank c = read [c] - 1
parseMove _ = Nothing

-- | Show the board with ANSI colors
showBoardWithColors :: Board -> String
showBoardWithColors board =
  "  a b c d e f g h\n" ++
  unlines [ show (8 - rank) ++ " " ++
            concatMap (\file -> colorSquare (file, 7 - rank) (getPiece (file, 7 - rank) board)) [0..7] ++
            " " ++ show (8 - rank)
          | rank <- [0..7]
          ] ++
  "  a b c d e f g h"
  where
    colorSquare pos piece =
      let squareColor = if (fst pos + snd pos) `mod` 2 == 0
                        then SetBackgroundColor Dull ANSI.Black
                        else SetBackgroundColor Dull ANSI.White
          pieceColor = case piece of
                         Just p -> if pieceColor p == White
                                   then SetColor Foreground Vivid ANSI.Blue
                                   else SetColor Foreground Vivid ANSI.Red
                         Nothing -> SetColor Foreground Dull ANSI.Black
          pieceChar = case piece of
                        Just p -> showPiece p
                        Nothing -> " "
      in setSGRCode [squareColor, pieceColor] ++ pieceChar ++ " " ++ setSGRCode [Reset]
