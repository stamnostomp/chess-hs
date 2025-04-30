module Chess.Move
  ( Move(..)
  , MoveType(..)
  , executeMove
  , isLegalMove
  ) where

import Chess.Board
import Chess.Pieces
import Data.Maybe (isJust, fromMaybe)

-- | Type of special moves
data MoveType = Normal | Capture | Castle | EnPassant | Promotion PieceType
  deriving (Show, Eq)

-- | A chess move from one position to another
data Move = Move
  { moveFrom :: Position   -- ^ Source position
  , moveTo :: Position     -- ^ Target position
  , moveType :: MoveType   -- ^ Type of move (derived)
  } deriving (Show, Eq)

-- | Constructor for a move with default type
instance {-# OVERLAPPING #-} Show Move where
  show (Move from to _) =
    let fileToChar f = ['a'..'h'] !! f
        rankToChar r = ['1'..'8'] !! r
    in [fileToChar (fst from), rankToChar (snd from),
        fileToChar (fst to), rankToChar (snd to)]

-- | Constructor for a simple move
Move :: Position -> Position -> Move
Move from to = Move from to Normal

-- | Execute a move on a board
executeMove :: Move -> Board -> Board
executeMove (Move from to moveType) board =
  case moveType of
    Normal -> movePiece from to board

    Capture -> movePiece from to board

    Castle ->
      let kingFile = fst from
          rookFile = if fst to > kingFile then 7 else 0
          rookToFile = if fst to > kingFile then fst to - 1 else fst to + 1
          rank = snd from
          rookFrom = (rookFile, rank)
          rookTo = (rookToFile, rank)
      in movePiece rookFrom rookTo $ movePiece from to board

    EnPassant ->
      let capturedPawnPos = (fst to, snd from)
      in movePiece from to $ setPiece capturedPawnPos undefined board

    Promotion newType ->
      let piece = fromMaybe (error "No piece at source position") (getPiece from board)
          promotedPiece = Piece (pieceColor piece) newType
      in setPiece to promotedPiece $ setPiece from undefined board

-- | Check if a move is legal
isLegalMove :: Move -> GameState -> Bool
isLegalMove _ _ = True  -- Placeholder - to be implemented in Chess.Rules

-- | Check if a position is attacked by a specific color
isAttacked :: Board -> Position -> Color -> Bool
isAttacked _ _ _ = False  -- Placeholder - to be implemented in Chess.Rules
