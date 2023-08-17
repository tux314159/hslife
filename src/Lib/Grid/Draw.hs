{-# LANGUAGE FlexibleContexts #-}

module Lib.Grid.Draw
  ( drawGrid,
  )
where

import Control.Lens.Operators
import Control.Lens.At
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Reflection
import Lib.Grid
import Lib.Util
import SDL
import SDL.Util

-- | Render a grid.
drawGrid :: (MonadIO m, Given Renderer) => Grid Bool -> V2 Int -> m ()
drawGrid grid pos = do
  let renderer = given
  withColor renderer (gridColor grid) $ \r -> do
    mapM_ (drawRectLine r) hlines
    mapM_ (drawRectLine r) vlines
  withColor renderer (V4 124 252 0 0) $ \r ->
    mapM_
      (mapM_ $ \(x, y) -> when (gridState grid ^. _gridState . ix y ^?! ix x) $ squareAt r x y)
      $ genIndices (gsq ^. _x) (gsq ^. _y)
  where
    -- For drawing the grid
    gsq = gridSize (gridState grid)
    gsqsz = gridSqSize grid
    hlen = fromIntegral $ (gsqsz + 1) * gsq ^. _x + 1
    vlen = fromIntegral $ (gsqsz + 1) * gsq ^. _y + 1
    hlines_s =
      P
        <$> zipWith
          V2
          (replicate (1 + gsq ^. _y) $ pos ^. _x)
          (iterate (+ (gsqsz + 1)) $ pos ^. _y)
    vlines_s =
      P
        <$> zipWith
          V2
          (iterate (+ (gsqsz + 1)) $ pos ^. _x)
          (replicate (1 + gsq ^. _x) $ pos ^. _y)
    hlines = map (`LineH` hlen) $ fmap fromIntegral <$> hlines_s
    vlines = map (`LineV` vlen) $ fmap fromIntegral <$> vlines_s
    -- For drawing individual cells
    squarePos x y =
      fromIntegral <$> V2 (x * (gsqsz + 1) + pos ^. _x + 2) (y * (gsqsz + 1) + pos ^. _y + 2)
    squareAt r x y =
      fillRect r . Just $
        Rectangle (P $ squarePos x y) (V2 (fromIntegral gsqsz - 2) (fromIntegral gsqsz - 2))
