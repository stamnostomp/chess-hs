module Chess.Pieces
  ( Color(..)
  , PieceType(..)
  , Piece(..)
  , opponent
  , showPiece
  ) where

-- | Color of a chess piece
data Color = White | Black
  deriving (Show, Eq)

-- | Type of chess piece
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Show, Eq)

-- | A chess piece with a color and type
data Piece = Piece
  { pieceColor :: Color
  , pieceType :: PieceType
  } deriving (Show, Eq)

-- | Get the opponent of a color
opponent :: Color -> Color
opponent White = Black
opponent Black = White

-- | Get the unicode character for a piece
showPiece :: Piece -> String
showPiece (Piece White Pawn)   = "♙"
showPiece (Piece White Knight) = "♘"
showPiece (Piece White Bishop) = "♗"
showPiece (Piece White Rook)   = "♖"
showPiece (Piece White Queen)  = "♕"
showPiece (Piece White King)   = "♔"
showPiece (Piece Black Pawn)   = "♟"
showPiece (Piece Black Knight) = "♞"
showPiece (Piece Black Bishop) = "♝"
showPiece (Piece Black Rook)   = "♜"
showPiece (Piece Black Queen)  = "♛"
showPiece (Piece Black King)   = "♚"
