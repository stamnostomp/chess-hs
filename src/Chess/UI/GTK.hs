module Chess.UI.GTK (run) where

import Control.Monad
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (isJust, fromMaybe)
import GI.Cairo.Render (setSourceRGB, rectangle, fill)
import qualified GI.Cairo.Render as Cairo
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Data.Text (Text, pack)
import qualified GI.Pango as Pango

import Chess.Board
import Chess.Game
import Chess.GameState
import Chess.Move
import Chess.Pieces
import Chess.Rules (isLegalMove)

-- | Run the GTK UI
run :: IO ()
run = do
  Gtk.init Nothing

  -- Create window
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle window (pack "Chess")
  Gtk.windowSetDefaultSize window 400 400
  Gtk.windowSetPosition window Gtk.WindowPositionCenter

  -- Create drawing area
  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest drawingArea 400 400

  -- Create game state
  gameState <- newIORef initialGameState
  selectedPos <- newIORef Nothing

  -- Handle drawing
  Gtk.onWidgetDraw drawingArea $ \context -> do
    game <- readIORef gameState
    selected <- readIORef selectedPos
    renderBoard context (gameBoard game) selected
    return True

  -- Handle mouse clicks
  Gtk.addEvents drawingArea [Gdk.EventMaskButtonPressMask]
  Gtk.onWidgetButtonPressEvent drawingArea $ \event -> do
    -- Get click position
    x <- Gdk.getEventButtonX event
    y <- Gdk.getEventButtonY event

    -- Convert to board coordinates
    let boardPos = pixelToBoard (x, y)

    -- Handle click
    when (isValidPosition boardPos) $ do
      game <- readIORef gameState
      sel <- readIORef selectedPos

      case sel of
        -- No piece selected, try to select one
        Nothing -> do
          let piece = getPiece boardPos (gameBoard game)
          case piece of
            Just p | pieceColor p == currentPlayer game -> do
              writeIORef selectedPos (Just boardPos)
            _ -> return ()

        -- Piece already selected, try to move it
        Just fromPos -> do
          if fromPos == boardPos
            then writeIORef selectedPos Nothing  -- Deselect
            else do
              let move = Move fromPos boardPos Normal
              if isLegalMove move game
                then do
                  let newGame = makeMove move game
                  writeIORef gameState newGame
                  writeIORef selectedPos Nothing
                else do
                  -- If clicked on own piece, select that instead
                  let piece = getPiece boardPos (gameBoard game)
                  case piece of
                    Just p | pieceColor p == currentPlayer game ->
                      writeIORef selectedPos (Just boardPos)
                    _ -> return ()

      Gtk.widgetQueueDraw drawingArea

    return True

  -- Set up UI
  Gtk.containerAdd window drawingArea

  -- Show window
  Gtk.widgetShowAll window
  Gtk.onWidgetDestroy window Gtk.mainQuit

  -- Start main loop
  Gtk.main

-- | Convert pixel coordinates to board position
pixelToBoard :: (Double, Double) -> Position
pixelToBoard (x, y) =
  let squareSize = 50
      file = floor (x / squareSize)
      rank = 7 - floor (y / squareSize)
  in (file, rank)

-- | Render the chess board
renderBoard :: Gtk.DrawingContext -> Board -> Maybe Position -> IO ()
renderBoard context board selected = do
  Cairo.renderWithContext context $ do
    -- Draw the squares
    forM_ [0..7] $ \rank -> do
      forM_ [0..7] $ \file -> do
        let isLight = (file + rank) `mod` 2 == 0
            isSelected = selected == Just (file, rank)
            color = if isSelected
                    then (0.9, 0.9, 0.5)
                    else if isLight
                         then (0.9, 0.9, 0.7)
                         else (0.5, 0.3, 0.1)

        setSourceRGB (fst3 color) (snd3 color) (thd3 color)
        rectangle (fromIntegral $ file * 50)
                  (fromIntegral $ (7 - rank) * 50)
                  (fromIntegral 50)
                  (fromIntegral 50)
        fill

    -- Draw the pieces
    forM_ (Map.toList board) $ \((file, rank), piece) -> do
      let x = fromIntegral $ file * 50 + 25
          y = fromIntegral $ (7 - rank) * 50 + 25
          pieceText = showPiece piece

      Cairo.save
      Cairo.translate x y

      -- Set color based on piece color
      case pieceColor piece of
        White -> setSourceRGB 1.0 1.0 1.0
        Black -> setSourceRGB 0.0 0.0 0.0

      -- Draw piece using Cairo
      Cairo.moveTo (-15) 15
      Cairo.setFontSize 32
      Cairo.showText (pack pieceText)

      Cairo.restore

-- | Helper functions for triple access
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
