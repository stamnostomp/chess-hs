module Chess.Move
  ( Move(..)
  , MoveType(..)
  , executeMove
  , makeMoveOnBoard
  ) where

import Chess.Board
import Chess.Pieces
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map as Map

-- | Type of special moves
data MoveType = Normal | Capture | Castle | EnPassant | Promotion PieceType
  deriving (Show, Eq)

-- | A chess move from one position to another
data Move = Move
  { moveFrom :: Position   -- ^ Source position
  , moveTo :: Position     -- ^ Target position
  , moveType :: MoveType   -- ^ Type of move (derived)
  } deriving (Eq)  -- Remove Show from deriving clause

-- | Custom Show instance for Move
instance Show Move where
  show (Move from to _) =
    let fileToChar f = ['a'..'h'] !! f
        rankToChar r = ['1'..'8'] !! r
    in [fileToChar (fst from), rankToChar (snd from),
        fileToChar (fst to), rankToChar (snd to)]

-- | Constructor for a simple move with Normal type
move :: Position -> Position -> Move
move from to = Move from to Normal

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
          pieceAtCapture = getPiece capturedPawnPos board
      in movePiece from to $
         case pieceAtCapture of
           Just _ -> Map.delete capturedPawnPos board
           Nothing -> board

    Promotion newType ->
      let piece = fromMaybe (error "No piece at source position") (getPiece from board)
          promotedPiece = Piece (pieceColor piece) newType
      in setPiece to promotedPiece $ Map.delete from board

-- | Make a move on the board (without validation)
makeMoveOnBoard :: Move -> Board -> Board
makeMoveOnBoard (Move from to _) board =
  case getPiece from board of
    Nothing -> board
    Just piece ->
      let boardAfterMove = Map.delete from (Map.insert to piece board)
      in boardAfterMove
