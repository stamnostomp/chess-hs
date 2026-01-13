module Chess.UI.GTK (run) where

import Control.Monad
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (isJust, fromMaybe)
import qualified GI.Cairo as Cairo
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.GLib as GLib
import Data.GI.Base
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
  Gtk.init

  -- Create window
  window <- Gtk.windowNew
  Gtk.windowSetTitle window (Just (pack "Chess"))
  Gtk.windowSetDefaultSize window 400 400

  -- Create drawing area
  drawingArea <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest drawingArea 400 400

  -- Create game state
  gameState <- newIORef initialGameState
  selectedPos <- newIORef Nothing
  legalMoves <- newIORef []
  warningFlash <- newIORef (0, 0, 0, 0) -- RGBA

  -- Handle drawing
  Gtk.drawingAreaSetDrawFunc drawingArea $ Just $ \_ context _ _ -> do
    game <- readIORef gameState
    selected <- readIORef selectedPos
    moves <- readIORef legalMoves
    flash <- readIORef warningFlash
    renderWithContext (drawBoard (gameBoard game) selected moves flash) context
    return ()

  -- Handle mouse clicks
  gesture <- Gtk.gestureClickNew
  Gtk.widgetAddController drawingArea gesture
  Gtk.onGestureClickPressed gesture $ \_ x y -> do

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

  -- Set up UI
  Gtk.windowSetChild window (Just drawingArea)

  -- Create main loop
  mainLoop <- GLib.mainLoopNew Nothing False

  -- Show window and drawing area
  Gtk.widgetShow drawingArea
  Gtk.widgetShow window
  Gtk.onWidgetDestroy window (GLib.mainLoopQuit mainLoop)

  -- Start main loop
  GLib.mainLoopRun mainLoop

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
          (r, g, b, a) = if isSelected
                         then (0.9, 0.9, 0.5, 1.0)
                         else if isLegal
                              then (0.5, 0.9, 0.5, 0.5) -- Green with 50% alpha
                              else if isLight
                                   then (0.9, 0.9, 0.7, 1.0)
                                   else (0.5, 0.3, 0.1, 1.0)

      -- Draw a square at this position
      C.setSourceRGBA r g b a
      C.rectangle (fromIntegral $ file * 50)
                (fromIntegral $ (7 - rank) * 50)
                (fromIntegral 50)
                (fromIntegral 50)
      C.fill

  -- Draw the pieces
  forM_ (Map.toList board) $ \((file, rank), piece) -> do
    let pieceText = showPiece piece
    C.setFontSize 40 -- Increased font size
    extents <- C.textExtents (pack pieceText)
    let textWidth = C.textExtentsWidth extents
        textHeight = C.textExtentsHeight extents
        x = fromIntegral file * 50 + 25 - textWidth / 2 - C.textExtentsXbearing extents
        y = fromIntegral (7 - rank) * 50 + (50 + textHeight) / 2

    -- Draw a piece at this position
    C.moveTo x y
    case pieceColor piece of
      White -> do
        C.setSourceRGB 1.0 1.0 1.0 -- White fill
        C.textPath (pack pieceText)
        C.fill
        C.setSourceRGB 0.0 0.0 0.0 -- Black outline
        C.setLineWidth 1.5 -- Slightly thicker outline
        C.textPath (pack pieceText)
        C.stroke
      Black -> do
        C.setSourceRGB 0.0 0.0 0.0 -- Black fill
        C.textPath (pack pieceText)
        C.fill
        C.setSourceRGB 1.0 1.0 1.0 -- White outline
        C.setLineWidth 1.5 -- Slightly thicker outline
        C.textPath (pack pieceText)
        C.stroke

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
