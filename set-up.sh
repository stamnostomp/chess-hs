#!/usr/bin/env sh




# Setup script for Chess-HS using Stack
set -e  # Exit on error

echo "Setting up Chess-HS with Stack"
echo "============================="

# Install stack if not present (uncomment if needed)
# curl -sSL https://get.haskellstack.org/ | sh

# Copy the fixed Board.hs file
cp src/Chess/Board.hs src/Chess/Board.hs.bak || true
cat > src/Chess/Board.hs << 'EOF'
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
EOF

# Install dependencies including GTK
echo "Installing dependencies..."
if command -v nix &> /dev/null; then
  echo "Nix detected, installing system dependencies..."
  nix-shell -p gtk3 gobject-introspection cairo pango glib --run "echo 'System dependencies installed.'"
else
  echo "Nix not found, please ensure GTK3 and related libraries are installed on your system."
  echo "On Debian/Ubuntu: sudo apt-get install libgtk-3-dev libcairo2-dev libpango1.0-dev libglib2.0-dev"
  echo "On Fedora: sudo dnf install gtk3-devel cairo-devel pango-devel glib2-devel"
  echo "On Arch: sudo pacman -S gtk3 cairo pango glib2"
fi

# Build the project
echo "Building with Stack..."
if [[ "$1" == "--no-gtk" ]]; then
  stack build --flag chess-hs:-enable-gtk
else
  stack build
fi

# Run the project if build succeeds
if [ $? -eq 0 ]; then
  echo
  echo "Build successful!"
  echo "Run with: stack exec chess-hs"
  echo "For GTK interface: stack exec chess-hs -- --gtk"
  echo

  read -p "Run the chess game now? (y/n) " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [[ "$1" == "--no-gtk" ]]; then
      stack exec chess-hs
    else
      stack exec chess-hs -- --gtk
    fi
  fi
else
  echo
  echo "Build failed. Check the errors above."
  echo
  echo "If you're having GTK-related issues, try running with --no-gtk:"
  echo "  ./setup-stack.sh --no-gtk"
  exit 1
fi
