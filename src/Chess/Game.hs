module Chess.Game
  ( module Chess.GameState
  , initialGameState
  , makeMove
  , isGameOver
  , isInCheck
  , allPositions
  , boardPieces  -- Export the new functions
  , findKing
  ) where

import Chess.Board
import Chess.GameState
import Chess.Move
import Chess.Pieces
import Chess.Rules
import Data.Maybe (isJust, fromMaybe, catMaybes)
import qualified Data.Map as Map

-- | List of all valid board positions
allPositions :: [Position]
allPositions = [(file, rank) | file <- [0..7], rank <- [0..7]]

-- | Get all pieces on the board with their positions
boardPieces :: Board -> [(Position, Piece)]
boardPieces = Map.toList

-- | Find the king of a specific color on the board
findKing :: Board -> Color -> Maybe Position
findKing board color =
  let kingPositions = [pos | (pos, piece) <- boardPieces board,
                             pieceColor piece == color &&
                             pieceType piece == King]
  in case kingPositions of
       [] -> Nothing      -- King not found (shouldn't happen in a valid game)
       (pos:_) -> Just pos  -- Return the first (and only) king position

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

-- | Make a move and update the game state
makeMove :: Move -> GameState -> GameState
makeMove move game =
  let newBoard = executeMove move (gameBoard game)
      newTurn = opponent (gameTurn game)
      newMoves = (moveFrom move, moveTo move) : gameMoves game

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

-- | Check if the game is over
isGameOver :: GameState -> Bool
isGameOver game = gameStatus game `elem` [Checkmate, Stalemate]

-- | Calculate whether the current player is in check
isInCheck :: GameState -> Bool
isInCheck game =
  let kingPos = findKing (gameBoard game) (gameTurn game)
  in case kingPos of
       Nothing -> False  -- This shouldn't happen in a valid game
       Just pos -> isAttacked (gameBoard game) pos (opponent (gameTurn game))

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
          [Move from to Normal | to <- allPositions, isLegalMove (Move from to Normal) game]) playerPieces
  in not (null allPossibleMoves)

-- | Update castling rights when a king or rook moves
updateCastlingRights :: Color -> Position -> (Bool, Bool) -> (Bool, Bool)
updateCastlingRights _ _ rights = rights  -- Placeholder implementation

-- | Get the en passant target after a move
getEnPassantTarget :: Move -> Board -> Maybe Position
getEnPassantTarget _ _ = Nothing  -- Placeholder implementation
