module Chess.Rules
  ( isLegalMove
  , getValidMoves
  , isAttacked
  ) where

import Chess.Board
import Chess.Pieces
import Chess.Move
import Data.Maybe (isJust, fromMaybe, catMaybes)

-- | Get all valid moves for a piece at a position
getValidMoves :: GameState -> Position -> [Position]
getValidMoves game pos =
  case getPiece pos (gameBoard game) of
    Nothing -> []
    Just piece ->
      let possibleMoves = getPossibleMoves (gameBoard game) pos piece
      in filter (\to -> isLegalMove (Move pos to) game) possibleMoves

-- | Check if a move is legal
isLegalMove :: Move -> GameState -> Bool
isLegalMove (Move from to _) game =
  -- Check if positions are valid
  isValidPosition from && isValidPosition to &&

  -- Check if there is a piece at the source position
  isJust (getPiece from (gameBoard game)) &&

  -- Check if the piece belongs to the current player
  let piece = fromMaybe (error "No piece at source position") (getPiece from (gameBoard game))
  in pieceColor piece == currentPlayer game &&

  -- Check if the move is valid for this piece type
  to `elem` getPossibleMoves (gameBoard game) from piece &&

  -- Check that the move doesn't leave the king in check
  not (causesCheck from to game)

-- | Check if a move would leave the king in check
causesCheck :: Position -> Position -> GameState -> Bool
causesCheck from to game =
  let board = gameBoard game
      player = currentPlayer game
      newBoard = executeMove (Move from to) board
      kingPos = findKing newBoard player
  in case kingPos of
       Nothing -> True  -- If king is not found, consider it illegal
       Just pos -> isAttacked newBoard pos (opponent player)

-- | Find the king of a specific color on the board
findKing :: Board -> Color -> Maybe Position
findKing board color = findPiece board (\p -> pieceType p == King && pieceColor p == color)

-- | Find a piece matching a predicate
findPiece :: Board -> (Piece -> Bool) -> Maybe Position
findPiece board predicate =
  let pieces = [(pos, piece) | (pos, piece) <- boardPieces board, predicate piece]
  in case pieces of
       [] -> Nothing
       ((pos, _):_) -> Just pos

-- | Get all pieces on the board with their positions
boardPieces :: Board -> [(Position, Piece)]
boardPieces board =
  [(pos, piece) | pos <- allPositions,
                  let maybePiece = getPiece pos board,
                  isJust maybePiece,
                  let piece = fromMaybe (error "No piece") maybePiece]

-- | Get all positions on the board
allPositions :: [Position]
allPositions = [(file, rank) | file <- [0..7], rank <- [0..7]]

-- | Get all possible moves for a piece without considering check
getPossibleMoves :: Board -> Position -> Piece -> [Position]
getPossibleMoves board (file, rank) (Piece color pieceType) =
  case pieceType of
    Pawn -> getPawnMoves board (file, rank) color
    Knight -> getKnightMoves board (file, rank) color
    Bishop -> getBishopMoves board (file, rank) color
    Rook -> getRookMoves board (file, rank) color
    Queen -> getQueenMoves board (file, rank) color
    King -> getKingMoves board (file, rank) color

-- | Get possible pawn moves
getPawnMoves :: Board -> Position -> Color -> [Position]
getPawnMoves board (file, rank) color =
  let direction = if color == White then 1 else -1
      startRank = if color == White then 1 else 6
      oneStep = (file, rank + direction)
      twoStep = (file, rank + 2 * direction)
      captureLeft = (file - 1, rank + direction)
      captureRight = (file + 1, rank + direction)

      -- Basic moves
      basicMoves =
        -- One step forward if square is empty
        (if isValidPosition oneStep && not (isJust (getPiece oneStep board))
         then [oneStep] else []) ++
        -- Two steps forward from starting position if both squares are empty
        (if rank == startRank &&
            isValidPosition twoStep &&
            not (isJust (getPiece oneStep board)) &&
            not (isJust (getPiece twoStep board))
         then [twoStep] else [])

      -- Captures
      captureMoves =
        -- Capture to the left
        (if isValidPosition captureLeft &&
            isJust (getPiece captureLeft board) &&
            pieceColor (fromMaybe (error "No piece") (getPiece captureLeft board)) /= color
         then [captureLeft] else []) ++
        -- Capture to the right
        (if isValidPosition captureRight &&
            isJust (getPiece captureRight board) &&
            pieceColor (fromMaybe (error "No piece") (getPiece captureRight board)) /= color
         then [captureRight] else [])

      -- En passant captures would be added here
  in basicMoves ++ captureMoves

-- | Get possible knight moves
getKnightMoves :: Board -> Position -> Color -> [Position]
getKnightMoves board (file, rank) color =
  let knightOffsets = [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]
      possibleMoves = [(file + df, rank + dr) | (df, dr) <- knightOffsets]
      validMoves = filter isValidPosition possibleMoves
  in filter (\pos ->
       case getPiece pos board of
         Nothing -> True  -- Empty square
         Just piece -> pieceColor piece /= color  -- Occupied by opponent
     ) validMoves

-- | Get possible bishop moves
getBishopMoves :: Board -> Position -> Color -> [Position]
getBishopMoves board pos color =
  getLongRangeMoves board pos color [(1,1), (1,-1), (-1,-1), (-1,1)]

-- | Get possible rook moves
getRookMoves :: Board -> Position -> Color -> [Position]
getRookMoves board pos color =
  getLongRangeMoves board pos color [(0,1), (1,0), (0,-1), (-1,0)]

-- | Get possible queen moves
getQueenMoves :: Board -> Position -> Color -> [Position]
getQueenMoves board pos color =
  getBishopMoves board pos color ++ getRookMoves board pos color

-- | Get possible king moves
getKingMoves :: Board -> Position -> Color -> [Position]
getKingMoves board (file, rank) color =
  let kingOffsets = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]
      possibleMoves = [(file + df, rank + dr) | (df, dr) <- kingOffsets]
      validMoves = filter isValidPosition possibleMoves
  in filter (\pos ->
       case getPiece pos board of
         Nothing -> True  -- Empty square
         Just piece -> pieceColor piece /= color  -- Occupied by opponent
     ) validMoves
     -- Castling would be added here

-- | Helper function for long range pieces (bishop, rook, queen)
getLongRangeMoves :: Board -> Position -> Color -> [(Int, Int)] -> [Position]
getLongRangeMoves board (file, rank) color directions =
  concatMap (\(df, dr) -> getMovesInDirection board (file, rank) color df dr) directions

-- | Get moves in a specific direction until blocked
getMovesInDirection :: Board -> Position -> Color -> Int -> Int -> [Position]
getMovesInDirection board (file, rank) color df dr =
  let nextPos = (file + df, rank + dr)
  in if not (isValidPosition nextPos)
     then []
     else case getPiece nextPos board of
            Nothing -> nextPos : getMovesInDirection board nextPos color df dr
            Just piece -> if pieceColor piece /= color
                          then [nextPos]  -- Can capture opponent's piece
                          else []  -- Blocked by own piece

-- | Check if a position is attacked by a specific color
isAttacked :: Board -> Position -> Color -> Bool
isAttacked board pos attackerColor =
  let
    -- Check if any pawn attacks the position
    pawnAttacks =
      let direction = if attackerColor == White then 1 else -1
          pawnPositions = [ (fst pos + df, snd pos - direction)
                          | df <- [-1, 1] ]
          validPawnPositions = filter isValidPosition pawnPositions
      in any (\pawnPos ->
            case getPiece pawnPos board of
              Just (Piece c Pawn) | c == attackerColor -> True
              _ -> False
          ) validPawnPositions

    -- Check if any knight attacks the position
    knightAttacks =
      let knightOffsets = [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]
          knightPositions = [(fst pos + df, snd pos + dr) | (df, dr) <- knightOffsets]
          validKnightPositions = filter isValidPosition knightPositions
      in any (\knightPos ->
            case getPiece knightPos board of
              Just (Piece c Knight) | c == attackerColor -> True
              _ -> False
          ) validKnightPositions

    -- Check if any bishop, rook, or queen attacks along their lines
    bishopDirections = [(1,1), (1,-1), (-1,-1), (-1,1)]
    rookDirections = [(0,1), (1,0), (0,-1), (-1,0)]

    checkDirection (df, dr) =
      let checkPos = (fst pos + df, snd pos + dr)
      in if not (isValidPosition checkPos)
         then False
         else case getPiece checkPos board of
                Nothing -> checkDirection (df + signum df, dr + signum dr)
                Just (Piece c pieceType) ->
                  c == attackerColor &&
                  (pieceType == Queen ||
                   (pieceType == Bishop && (df, dr) `elem` bishopDirections) ||
                   (pieceType == Rook && (df, dr) `elem` rookDirections))
                _ -> False

    directionAttacks = any checkDirection (bishopDirections ++ rookDirections)

    -- Check if any king attacks the position
    kingAttacks =
      let kingOffsets = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]
          kingPositions = [(fst pos + df, snd pos + dr) | (df, dr) <- kingOffsets]
          validKingPositions = filter isValidPosition kingPositions
      in any (\kingPos ->
            case getPiece kingPos board of
              Just (Piece c King) | c == attackerColor -> True
              _ -> False
          ) validKingPositions
  in
    pawnAttacks || knightAttacks || directionAttacks || kingAttacks
