module Chess.Game
  ( GameState(..)
  , GameStatus(..)
  , initialGameState
  , currentPlayer
  , makeMove
  , isGameOver
  , isInCheck
  , getWinner
  ) where

import Chess.Board
import Chess.Move
import Chess.Pieces
import Chess.Rules
import Data.Maybe (isJust)

-- | Status of the game
data GameStatus = InProgress | Check | Checkmate | Stalemate
  deriving (Show, Eq)

-- | State of a chess game
data GameState = GameState
  { gameBoard :: Board            -- ^ Current board state
  , gameTurn :: Color             -- ^ Current player's turn
  , gameMoves :: [Move]           -- ^ List of moves made
  , gameStatus :: GameStatus      -- ^ Current game status
  , gameEnPassantTarget :: Maybe Position  -- ^ Position for en passant capture, if any
  , gameWhiteCastlingRights :: (Bool, Bool)  -- ^ Can white castle (kingside, queenside)
  , gameBlackCastlingRights :: (Bool, Bool)  -- ^ Can black castle (kingside, queenside)
  } deriving (Show)

-- | Initial game state
initialGameState :: GameState
initialGameState = GameState
  { gameBoard = initialBoard
  , gameTurn = White
  , gameMoves = []
  , gameStatus = InProgress
  , gameEnPassantTarget = Nothing
  , gameWhiteCastlingRights = (True, True)
  , gameBlackCastlingRights = (True, True)
  }

-- | Get the current player
currentPlayer :: GameState -> Color
currentPlayer = gameTurn

-- | Make a move and update the game state
makeMove :: Move -> GameState -> GameState
makeMove move game =
  let newBoard = executeMove move (gameBoard game)
      newTurn = opponent (gameTurn game)
      newMoves = move : gameMoves game

      -- Update castling rights when king or rook moves
      newWhiteCastlingRights = updateCastlingRights White (moveFrom move) (gameWhiteCastlingRights game)
      newBlackCastlingRights = updateCastlingRights Black (moveFrom move) (gameBlackCastlingRights game)

      -- Update en passant target
      newEnPassantTarget = getEnPassantTarget move (gameBoard game)

      -- Create temporary game state
      tempGame = game
        { gameBoard = newBoard
        , gameTurn = newTurn
        , gameMoves = newMoves
        , gameEnPassantTarget = newEnPassantTarget
        , gameWhiteCastlingRights = newWhiteCastlingRights
        , gameBlackCastlingRights = newBlackCastlingRights
        }

      -- Update game status
      newStatus = calculateGameStatus tempGame
  in
    tempGame { gameStatus = newStatus }

-- | Calculate whether the current player is in check
isInCheck :: GameState -> Bool
isInCheck game =
  let kingPos = findKing (gameBoard game) (gameTurn game)
  in case kingPos of
       Nothing -> False  -- This shouldn't happen in a valid game
       Just pos -> isAttacked (gameBoard game) pos (opponent (gameTurn game))

-- | Check if the game is over
isGameOver :: GameState -> Bool
isGameOver game = gameStatus game `elem` [Checkmate, Stalemate]

-- | Get the winner of the game, if any
getWinner :: GameState -> Maybe Color
getWinner game = case gameStatus game of
  Checkmate -> Just (opponent (gameTurn game))
  _ -> Nothing

-- | Calculate the current game status
calculateGameStatus :: GameState -> GameStatus
calculateGameStatus game
  | not (hasLegalMoves game) =
      if isInCheck game then Checkmate else Stalemate
  | isInCheck game = Check
  | otherwise = InProgress

-- | Check if the current player has any legal moves
hasLegalMoves :: GameState -> Bool
hasLegalMoves game =
  let player = gameTurn game
      playerPieces = [(pos, piece) | (pos, piece) <- boardPieces (gameBoard game), pieceColor piece == player]
      allPossibleMoves = concatMap (\(from, _) ->
          [Move from to | to <- allPositions, isLegalMove (Move from to) game]) playerPieces
  in not (null allPossibleMoves)

-- | Get all pieces on the board with their positions
boardPieces :: Board -> [(Position, Piece)]
boardPieces = undefined -- To be implemented

-- | Get all valid positions on the board
allPositions :: [Position]
allPositions = [(file, rank) | file <- [0..7], rank <- [0..7]]

-- | Find the king of the given color
findKing :: Board -> Color -> Maybe Position
findKing = undefined -- To be implemented

-- | Check if a position is attacked by the opponent
isAttacked :: Board -> Position -> Color -> Bool
isAttacked = undefined -- To be implemented

-- | Update castling rights when a king or rook moves
updateCastlingRights :: Color -> Position -> (Bool, Bool) -> (Bool, Bool)
updateCastlingRights = undefined -- To be implemented

-- | Get the en passant target after a move
getEnPassantTarget :: Move -> Board -> Maybe Position
getEnPassantTarget = undefined -- To be implemented
