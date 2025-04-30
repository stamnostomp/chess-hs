module Chess.GameState
  ( GameState(..)
  , GameStatus(..)
  , currentPlayer
  , getWinner
  ) where

import Chess.Board
import Chess.Pieces

-- | Status of the game
data GameStatus = InProgress | Check | Checkmate | Stalemate
  deriving (Show, Eq)

-- | State of a chess game
data GameState = GameState
  { gameBoard :: Board            -- ^ Current board state
  , gameTurn :: Color             -- ^ Current player's turn
  , gameMoves :: [(Position, Position)]  -- ^ List of moves made (from, to)
  , gameStatus :: GameStatus      -- ^ Current game status
  , gameEnPassantTarget :: Maybe Position  -- ^ Position for en passant capture, if any
  , gameWhiteCastlingRights :: (Bool, Bool)  -- ^ Can white castle (kingside, queenside)
  , gameBlackCastlingRights :: (Bool, Bool)  -- ^ Can black castle (kingside, queenside)
  } deriving (Show)

-- | Get the current player
currentPlayer :: GameState -> Color
currentPlayer = gameTurn

-- | Get the winner of the game, if any
getWinner :: GameState -> Maybe Color
getWinner game = case gameStatus game of
  Checkmate -> Just (opponent (gameTurn game))
  _ -> Nothing
