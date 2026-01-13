module Chess.UI.Terminal (run) where

import Control.Monad (when)
import System.Console.ANSI
import System.IO
import Data.Char (toLower)
import Data.Maybe (fromMaybe, fromJust, isJust)

import Chess.Board
import Chess.Game
import Chess.GameState
import Chess.Move
import qualified Chess.Pieces as P -- Import qualified to avoid name clashes
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
        Checkmate -> "Checkmate! " ++ show (fromMaybe (P.opponent (gameTurn game)) (getWinner game)) ++ " wins!"
        Stalemate -> "Stalemate! The game is a draw."
        _ -> "Game over."
    else do
      -- Get the player's move
      putStr $ "\n" ++ show (gameTurn game) ++ "'s move: "
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

  -- Get terminal width for centering
  maybeSize <- getTerminalSize
  let termWidth = maybe 80 snd maybeSize
      boardWidth = 64  -- Width of the board display
      padding = replicate (max 0 ((termWidth - boardWidth) `div` 2)) ' '

  putStrLn $ padding ++ "Turn: " ++ show (gameTurn game) ++
             case gameStatus game of
               InProgress -> ""
               Check -> " (Check!)"
               _ -> " (Game over)"

  putStr $ showBoardWithColors padding (gameBoard game)

  -- Show move history
  when (not (null (gameMoves game))) $ do
    putStrLn $ padding ++ "Recent moves:"
    putStr padding
    mapM_ (putStr . (++ " ")) (take 10 (map show (reverse (gameMoves game))))
    putStrLn ""

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
showBoardWithColors :: String -> Board -> String
showBoardWithColors pad board =
  "\n" ++ pad ++
  "     a      b      c      d      e      f      g      h\n" ++
  concatMap (\rank ->
    -- Top line of square
    pad ++ "   " ++ concatMap (\file -> colorSquare (file, 7 - rank) Nothing) [0..7] ++ "\n" ++
    -- Middle line with piece
    pad ++ show (8 - rank) ++ "  " ++ concatMap (\file -> colorSquare (file, 7 - rank) (getPiece (file, 7 - rank) board)) [0..7] ++ "  " ++ show (8 - rank) ++ "\n" ++
    -- Bottom line of square
    pad ++ "   " ++ concatMap (\file -> colorSquare (file, 7 - rank) Nothing) [0..7] ++ "\n"
  ) [0..7] ++
  pad ++ "     a      b      c      d      e      f      g      h\n"
  where
    colorSquare pos piece =
      let squareColor = if (fst pos + snd pos) `mod` 2 == 0
                        then SetColor Background Dull Black
                        else SetColor Background Dull White
          textColor = case piece of
                         Just p -> if P.pieceColor p == P.White
                                   then SetColor Foreground Vivid Blue
                                   else SetColor Foreground Vivid Red
                         Nothing -> SetColor Foreground Dull Black
          pieceChar = case piece of
                        Just p -> P.showPiece p
                        Nothing -> " "
      in setSGRCode [squareColor, textColor] ++ "   " ++ pieceChar ++ "   " ++ setSGRCode [Reset]
