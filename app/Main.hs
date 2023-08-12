{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Vector ((!))
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Life.State
import SDL
import Util (genIndices)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = AcceleratedRenderer}
  appLoop renderer $ gliderLife $ V2 25 25
  destroyWindow window

type Color = V4 Word8

appLoop :: Renderer -> LifeState -> IO ()
appLoop renderer state = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  drawGrid renderer (Grid (V4 0 0 0 255) (V2 25 25) 20) state (V2 0 0)
  present renderer
  delay 50
  unless qPressed (appLoop renderer $ lifeStep state)

data Grid = Grid
  { -- | Color of the lines
    gridColor :: Color,
    -- | Number of squares along each edge (x, y)
    gridSquares :: V2 CInt,
    -- | Size of each square (edgelength)
    gridSqSize :: CInt
  }

withColor :: (MonadIO m) => Renderer -> Color -> (Renderer -> m ()) -> m ()
withColor renderer color f = do
  old <- get $ rendererDrawColor renderer
  rendererDrawColor renderer $= color
  f renderer
  rendererDrawColor renderer $= old

-- | Line{horiz/vert} start length
data Line = LineH (Point V2 CInt) CInt | LineV (Point V2 CInt) CInt

drawGrid :: (MonadIO m) => Renderer -> Grid -> LifeState -> V2 CInt -> m ()
drawGrid renderer grid state pos = do
  withColor renderer (gridColor grid) $ \r -> do
    mapM_ (drawRectLine r) hlines
    mapM_ (drawRectLine r) vlines
  withColor renderer (V4 124 252 0 0) $ \r ->
    mapM_
      (mapM_ $ \(x, y) -> when (state ! fromIntegral y ! fromIntegral x) $ squareAt r x y)
      $ genIndices (gsq ^. _x) (gsq ^. _y)
  where
    -- Lines of thickness exactly 1
    drawRectLine r (LineH start len) = fillRect r $ Just $ Rectangle start (V2 len 1)
    drawRectLine r (LineV start len) = fillRect r $ Just $ Rectangle start (V2 1 len)
    -- For drawing the grid
    gsq = gridSquares grid
    gsqsz = gridSqSize grid
    hlen = (gsqsz + 1) * gsq ^. _x + 1
    vlen = (gsqsz + 1) * gsq ^. _y + 1
    hlines_s =
      P
        <$> zipWith
          V2
          (replicate (fromIntegral $ 1 + gsq ^. _y) $ pos ^. _x)
          (iterate (+ (gsqsz + 1)) $ pos ^. _y)
    vlines_s =
      P
        <$> zipWith
          V2
          (iterate (+ (gsqsz + 1)) $ pos ^. _x)
          (replicate (fromIntegral $ 1 + gsq ^. _x) $ pos ^. _y)
    hlines = map (`LineH` hlen) hlines_s
    vlines = map (`LineV` vlen) vlines_s
    -- For drawing individual cells
    squarePos x y = V2 (x * (gsqsz + 1) + pos ^. _x + 2) (y * (gsqsz + 1) + pos ^. _y + 2)
    squareAt r x y = fillRect r $ Just $ Rectangle (P $ squarePos x y) (V2 (gsqsz - 2) (gsqsz - 2))
