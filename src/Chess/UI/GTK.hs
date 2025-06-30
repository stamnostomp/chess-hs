module Chess.UI.GTK (run) where

import Control.Monad
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (isJust, fromMaybe)
import qualified GI.Cairo as Cairo
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Data.Text (Text, pack)
import qualified GI.Pango as Pango

-- Import Cairo with qualified name to avoid name conflicts
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)
import Control.Monad.Trans.Reader (runReaderT)
import Data.GI.Base.ManagedPtr (withManagedPtr)

import Chess.Board
import Chess.Game
import Chess.GameState
import Chess.Move
import Chess.Pieces
import Chess.Rules (isLegalMove, getValidMoves)

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
  legalMoves <- newIORef []
  warningFlash <- newIORef (0, 0, 0, 0) -- RGBA

  -- Handle drawing
  Gtk.onWidgetDraw drawingArea $ \context -> do
    game <- readIORef gameState
    selected <- readIORef selectedPos
    moves <- readIORef legalMoves
    flash <- readIORef warningFlash
    renderWithContext (drawBoard (gameBoard game) selected moves flash) context
    return True

  -- Handle mouse clicks
  Gtk.widgetAddEvents drawingArea [Gdk.EventMaskButtonPressMask]
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
            Just p | pieceColor p == gameTurn game -> do
              writeIORef selectedPos (Just boardPos)
              let moves = getValidMoves game boardPos
              writeIORef legalMoves moves
            _ -> return ()

        -- Piece already selected, try to move it
        Just fromPos -> do
          if fromPos == boardPos
            then do
              writeIORef selectedPos Nothing  -- Deselect
              writeIORef legalMoves []
            else do
              let move = Move fromPos boardPos Normal
              if isLegalMove move game
                then do
                  let newGame = makeMove move game
                  writeIORef gameState newGame
                  writeIORef selectedPos Nothing
                  writeIORef legalMoves []
                else do
                  -- If clicked on own piece, select that instead
                  let piece = getPiece boardPos (gameBoard game)
                  case piece of
                    Just p | pieceColor p == gameTurn game -> do
                      writeIORef selectedPos (Just boardPos)
                      let moves = getValidMoves game boardPos
                      writeIORef legalMoves moves
                    _ -> do
                      writeIORef warningFlash (1, 0, 0, 0.5) -- Red flash

      Gtk.widgetQueueDraw drawingArea

    return True

  -- Set up UI
  Gtk.containerAdd window drawingArea

  -- Show window
  Gtk.widgetShowAll window
  Gtk.onWidgetDestroy window Gtk.mainQuit

  -- Start main loop
  Gtk.main

-- | This function bridges gi-cairo with the hand-written cairo package
renderWithContext :: Render () -> Cairo.Context -> IO Bool
renderWithContext r context = do
  withManagedPtr context $ \p ->
    runReaderT (runRender r) (Cairo (castPtr p))
  return True

-- | Convert pixel coordinates to board position
pixelToBoard :: (Double, Double) -> Position
pixelToBoard (x, y) =
  let squareSize = 50
      file = floor (x / squareSize)
      rank = 7 - floor (y / squareSize)
  in (file, rank)

-- | Draw the chess board using Cairo
drawBoard :: Board -> Maybe Position -> [Position] -> (Double, Double, Double, Double) -> Render ()
drawBoard board selected legalMoves (r, g, b, a) = do
  -- Draw the squares
  forM_ [0..7] $ \rank -> do
    forM_ [0..7] $ \file -> do
      let pos = (file, rank)
          isLight = (file + rank) `mod` 2 == 0
          isSelected = selected == Just pos
          isLegal = pos `elem` legalMoves
          color = if isSelected
                  then (0.9, 0.9, 0.5, 1.0)
                  else if isLegal
                       then (0.5, 0.9, 0.5, 0.5) -- Transparent green
                       else if isLight
                            then (0.9, 0.9, 0.7, 1.0)
                            else (0.5, 0.3, 0.1, 1.0)

      -- Draw a square at this position
      C.setSourceRGBA (fst4 color) (snd4 color) (thd4 color) (frth4 color)
      C.rectangle (fromIntegral $ file * 50)
                (fromIntegral $ (7 - rank) * 50)
                (fromIntegral 50)
                (fromIntegral 50)
      C.fill

  -- Draw the pieces
  forM_ (Map.toList board) $ \((file, rank), piece) -> do
    let x = fromIntegral $ file * 50 + 25
        y = fromIntegral $ (7 - rank) * 50 + 25
        pieceText = showPiece piece

    -- Draw a piece at this position
    C.save
    C.translate x y

    -- Set color based on piece color
    case pieceColor piece of
      White -> C.setSourceRGB 1.0 1.0 1.0
      Black -> C.setSourceRGB 0.0 0.0 0.0

    -- Draw piece using Cairo
    C.moveTo (-15) 15
    C.setFontSize 32
    C.showText (pack pieceText)
    C.fill -- Fill the text to make it solid

    C.restore

  -- Draw warning flash
  C.setSourceRGBA r g b a
  C.paint

-- | Helper functions for triple access
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | Helper functions for quadruple access
fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

thd4 :: (a, b, c, d) -> c
thd4 (_, _, c, _) = c

frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, d) = d
