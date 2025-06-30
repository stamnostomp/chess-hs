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
  , findKing
  , getPiecesByColor
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Chess.Pieces
import Data.Maybe (listToMaybe)

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
initialBoard = foldr addPiece emptyBoard allPieces
  where
    -- Helper function to add pieces to the board
    addPiece :: (Position, Piece) -> Board -> Board
    addPiece (pos, piece) board = setPiece pos piece board

    -- All pieces combined into a single list
    allPieces :: [(Position, Piece)]
    allPieces = whitePieces ++ blackPieces ++ whitePawns ++ blackPawns

    -- Define the pieces
    whitePieces :: [(Position, Piece)]
    whitePieces =
      [ ((0, 0), Piece White Rook)
      , ((1, 0), Piece White Knight)
      , ((2, 0), Piece White Bishop)
      , ((3, 0), Piece White Queen)
      , ((4, 0), Piece White King)
      , ((5, 0), Piece White Bishop)
      , ((6, 0), Piece White Knight)
      , ((7, 0), Piece White Rook)
      ]

    blackPieces :: [(Position, Piece)]
    blackPieces =
      [ ((0, 7), Piece Black Rook)
      , ((1, 7), Piece Black Knight)
      , ((2, 7), Piece Black Bishop)
      , ((3, 7), Piece Black Queen)
      , ((4, 7), Piece Black King)
      , ((5, 7), Piece Black Bishop)
      , ((6, 7), Piece Black Knight)
      , ((7, 7), Piece Black Rook)
      ]

    whitePawns :: [(Position, Piece)]
    whitePawns = [((i, 1), Piece White Pawn) | i <- [0..7]]

    blackPawns :: [(Position, Piece)]
    blackPawns = [((i, 6), Piece Black Pawn) | i <- [0..7]]

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
showBoard board =
  let -- Create the rank lines (one for each rank)
      rankLines = [concat [showSquare (file, rank) | file <- [0..7]] ++ " " ++ show (rank + 1) | rank <- [0..7]]
      -- Create the file labels line
      fileLabels = "abcdefgh"
      -- Combine into a complete board string
      boardLines = reverse rankLines ++ [fileLabels]
  in unlines boardLines
  where
    showSquare :: Position -> String
    showSquare pos = maybe "." showPiece (getPiece pos board)

-- | Find the king of a specific color
findKing :: Board -> Color -> Maybe Position
findKing board color =
  let isKing (pos, piece) = pieceType piece == King && pieceColor piece == color
  in fmap fst (listToMaybe (filter isKing (Map.toList board)))

-- | Get all pieces of a specific color
getPiecesByColor :: Board -> Color -> Map.Map Position Piece
getPiecesByColor board color = Map.filter (\p -> pieceColor p == color) board
