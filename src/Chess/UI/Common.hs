module Chess.UI.Common
  ( formatPosition
  , parsePosition
  , showGameStatus
  , formatMove
  , parseMove
  ) where

import Data.Char (toLower, toUpper, isDigit)
import Chess.Board
import Chess.GameState
import Chess.Move
import Chess.Pieces

-- | Format a position as a string (e.g., "e4")
formatPosition :: Position -> String
formatPosition (file, rank) =
  [['a'..'h'] !! file, ['1'..'8'] !! rank]

-- | Parse a position from a string (e.g., "e4")
parsePosition :: String -> Maybe Position
parsePosition [file, rank]
  | file `elem` ['a'..'h'] && rank `elem` ['1'..'8'] =
      Just (fromEnum file - fromEnum 'a', fromEnum rank - fromEnum '1')
  | file `elem` ['A'..'H'] && rank `elem` ['1'..'8'] =
      Just (fromEnum file - fromEnum 'A', fromEnum rank - fromEnum '1')
  | otherwise = Nothing
parsePosition _ = Nothing

-- | Format move in algebraic notation
formatMove :: Move -> String
formatMove (Move from to _) =
  formatPosition from ++ formatPosition to

-- | Parse move from algebraic notation
parseMove :: String -> Maybe Move
parseMove [f1, r1, f2, r2] =
  case (parsePosition [f1, r1], parsePosition [f2, r2]) of
    (Just from, Just to) -> Just (Move from to Normal)
    _ -> Nothing
parseMove _ = Nothing

-- | Show game status in a user-friendly format
showGameStatus :: GameStatus -> String
showGameStatus InProgress = "In Progress"
showGameStatus Check = "Check"
showGameStatus Checkmate = "Checkmate"
showGameStatus Stalemate = "Stalemate"
