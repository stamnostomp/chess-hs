module Chess.Rules
  ( isLegalMove
  , getValidMoves
  , isAttacked
  ) where

import Chess.Board (Board, Position, getPiece, isValidPosition, findKing, getPiecesByColor)
import Chess.Pieces
import Chess.Move (Move(..), MoveType(..), makeMoveOnBoard)
import Chess.GameState
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map as Map

-- | Get all valid moves for a piece at a position
getValidMoves :: GameState -> Position -> [Position]
getValidMoves game pos =
  case getPiece pos (gameBoard game) of
    Nothing -> []
    Just piece ->
      if pieceColor piece /= gameTurn game
      then []
      else
        let possibleMoves = getPossibleMoves (gameBoard game) pos piece
        in filter (\to -> isLegalMove (Move pos to Normal) game) possibleMoves

-- | Check if a move is legal
isLegalMove :: Move -> GameState -> Bool
isLegalMove move@(Move from to _) game =
  case getPiece from (gameBoard game) of
    Nothing -> False -- No piece to move
    Just piece ->
      -- Check if the piece belongs to the current player
      (pieceColor piece == gameTurn game) &&
      -- Check if the destination is valid for the piece type
      (to `elem` getPossibleMoves (gameBoard game) from piece) &&
      -- Check if the move is a capture of own piece
      (case getPiece to (gameBoard game) of
         Just targetPiece -> pieceColor targetPiece /= gameTurn game
         Nothing -> True) &&
      -- Check if the king is in check after the move
      (let
         -- Create a temporary board to see the result of the move
         tempBoard = makeMoveOnBoard move (gameBoard game)
         -- Find the king's position on the temporary board
         kingPos = fromMaybe (error "King not found!") (findKing tempBoard (gameTurn game))
         -- Check if the king is attacked on the temporary board
       in not (isAttacked tempBoard kingPos (opponent (gameTurn game))))

-- | Get possible moves based on piece type
getPossibleMoves :: Board -> Position -> Piece -> [Position]
getPossibleMoves board pos piece =
  case pieceType piece of
    Pawn -> getPawnMoves board pos (pieceColor piece)
    Knight -> getKnightMoves board pos (pieceColor piece)
    Bishop -> getBishopMoves board pos (pieceColor piece)
    Rook -> getRookMoves board pos (pieceColor piece)
    Queen -> getQueenMoves board pos (pieceColor piece)
    King -> getKingMoves board pos (pieceColor piece)

-- | Get possible pawn moves (simplified)
getPawnMoves :: Board -> Position -> Color -> [Position]
getPawnMoves board (file, rank) color =
  let direction = if color == White then 1 else -1
      startRank = if color == White then 1 else 6
      oneStep = (file, rank + direction)
      twoStep = (file, rank + 2 * direction)
      captureLeft = (file - 1, rank + direction)
      captureRight = (file + 1, rank + direction)

      -- Basic moves
      validOneStep = isValidPosition oneStep && not (isJust (getPiece oneStep board))
      validTwoStep = rank == startRank && validOneStep &&
                    isValidPosition twoStep && not (isJust (getPiece twoStep board))

      -- Captures
      validCaptureLeft = isValidPosition captureLeft &&
                         case getPiece captureLeft board of
                           Just p -> pieceColor p /= color
                           Nothing -> False
      validCaptureRight = isValidPosition captureRight &&
                          case getPiece captureRight board of
                            Just p -> pieceColor p /= color
                            Nothing -> False
  in
    (if validOneStep then [oneStep] else []) ++
    (if validTwoStep then [twoStep] else []) ++
    (if validCaptureLeft then [captureLeft] else []) ++
    (if validCaptureRight then [captureRight] else [])

-- | Get possible knight moves
getKnightMoves :: Board -> Position -> Color -> [Position]
getKnightMoves board (file, rank) color =
  let offsets = [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]
      positions = [(file + df, rank + dr) | (df, dr) <- offsets]
  in filter isValidPosition positions

-- | Get possible bishop moves
getBishopMoves :: Board -> Position -> Color -> [Position]
getBishopMoves board pos color =
  concatMap (getMovesInDirection board pos color) [(1,1), (1,-1), (-1,-1), (-1,1)]

-- | Get possible rook moves
getRookMoves :: Board -> Position -> Color -> [Position]
getRookMoves board pos color =
  concatMap (getMovesInDirection board pos color) [(0,1), (1,0), (0,-1), (-1,0)]

-- | Get possible queen moves
getQueenMoves :: Board -> Position -> Color -> [Position]
getQueenMoves board pos color =
  getBishopMoves board pos color ++ getRookMoves board pos color

-- | Get possible king moves
getKingMoves :: Board -> Position -> Color -> [Position]
getKingMoves board (file, rank) color =
  let offsets = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]
      positions = [(file + df, rank + dr) | (df, dr) <- offsets]
  in filter isValidPosition positions

-- | Get moves in a specific direction until blocked
getMovesInDirection :: Board -> Position -> Color -> (Int, Int) -> [Position]
getMovesInDirection board (file, rank) color (df, dr) =
  let next = (file + df, rank + dr)
  in
    if not (isValidPosition next)
    then []
    else
      case getPiece next board of
        Nothing -> next : getMovesInDirection board next color (df, dr)
        Just p -> if pieceColor p /= color
                 then [next]  -- Can capture opponent's piece
                 else []      -- Blocked by own piece

-- | Check if a position is attacked by a specific color
isAttacked :: Board -> Position -> Color -> Bool
isAttacked board pos attackerColor =
  any (isAttacking pos) (Map.keys (getPiecesByColor board attackerColor))

  where
    -- Check if a piece at a given position is attacking the target position
    isAttacking :: Position -> Position -> Bool
    isAttacking targetPos piecePos =
      case getPiece piecePos board of
        Nothing -> False
        Just piece ->
          let
            -- Get possible moves for the attacking piece
            moves = getPossibleMoves board piecePos piece
            -- Check if the target position is in the list of possible moves
          in targetPos `elem` moves
