module Chess.Board
  ( Board
  , Position
  , emptyBoard
  , initialBoard
  , getPiece
  , setPiece
  , movePiece
  , isValidPosition
  , showBoard
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Chess.Pieces

-- | A position on the chess board represented as (file, rank)
-- where file is a-h (0-7) and rank is 1-8 (0-7)
type Position = (Int, Int)

-- | The chess board represented as a map from positions to pieces
type Board = Map Position Piece

-- | An empty chess board
emptyBoard :: Board
emptyBoard = Map.empty

-- | Initial chess board setup
initialBoard :: Board
initialBoard = foldr (uncurry setPiece) emptyBoard
  -- White pieces
  [ ((0, 0), Piece White Rook)
  , ((1, 0), Piece White Knight)
  , ((2, 0), Piece White Bishop)
  , ((3, 0), Piece White Queen)
  , ((4, 0), Piece White King)
  , ((5, 0), Piece White Bishop)
  , ((6, 0), Piece White Knight)
  , ((7, 0), Piece White Rook)
  ] ++
  -- White pawns
  [ ((i, 1), Piece White Pawn) | i <- [0..7] ] ++
  -- Black pawns
  [ ((i, 6), Piece Black Pawn) | i <- [0..7] ] ++
  -- Black pieces
  [ ((0, 7), Piece Black Rook)
  , ((1, 7), Piece Black Knight)
  , ((2, 7), Piece Black Bishop)
  , ((3, 7), Piece Black Queen)
  , ((4, 7), Piece Black King)
  , ((5, 7), Piece Black Bishop)
  , ((6, 7), Piece Black Knight)
  , ((7, 7), Piece Black Rook)
  ]

-- | Get a piece at a position
getPiece :: Position -> Board -> Maybe Piece
getPiece = Map.lookup

-- | Set a piece at a position
setPiece :: Position -> Piece -> Board -> Board
setPiece = Map.insert

-- | Move a piece from one position to another
movePiece :: Position -> Position -> Board -> Board
movePiece from to board = case getPiece from board of
  Nothing -> board
  Just piece -> setPiece to piece $ Map.delete from board

-- | Check if a position is valid
isValidPosition :: Position -> Bool
isValidPosition (file, rank) = file >= 0 && file <= 7 && rank >= 0 && rank <= 7

-- | Convert a board to a string representation
showBoard :: Board -> String
showBoard board = unlines $ reverse
  [ concat [ showSquare (file, rank) | file <- [0..7] ] ++ " " ++ show (rank + 1)
  | rank <- [0..7]
  ] ++ "abcdefgh"
  where
    showSquare pos = maybe "." showPiece (getPiece pos board)
