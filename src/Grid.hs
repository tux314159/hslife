{-# LANGUAGE GADTs #-}

module Grid
  ( Grid (..),
    GridState (..),
  )
where

import Linear.V2 (V2 (..))
import SDL.Util

-- | Current state of the grid
class GridState s where
  -- | Get state of a particular square
  gridStateAt :: (Integral a) => s -> a -> a -> Bool

  -- | Get size of grid in squares
  gridSize :: s -> V2 Int

-- | A grid of square cells
data Grid state = (GridState state) =>
  Grid
  { -- | Color of the lines
    gridColor :: Color,
    -- | Size of each square (edgelength)
    gridSqSize :: Int,
    -- | State of the grid
    gridState :: state
  }
