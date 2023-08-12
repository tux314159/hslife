module Grid where

import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Vector ((!))
import Foreign.C.Types (CInt)
import Life.State
import SDL
import SDL.Util
import Util (genIndices)

data Grid = Grid
  { -- | Color of the lines
    gridColor :: Color,
    -- | Number of squares along each edge (x, y)
    gridSquares :: V2 CInt,
    -- | Size of each square (edgelength)
    gridSqSize :: CInt
  }

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
