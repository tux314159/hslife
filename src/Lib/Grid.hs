module Lib.Grid
  ( Grid (..),
    GridState (..),
    _gridState,
    gridSize
  )
where

import Control.Lens.Operators
import Control.Lens.At
import qualified Data.Vector as V
import Linear.V2 (V2 (..))
import SDL.Util

type VV a = V.Vector (V.Vector a)

-- | Current state of the grid
newtype GridState a = GridState (VV a)

-- | Get state of a particular square
_gridState :: Functor f => (VV a -> f (VV a)) -> GridState a -> f (GridState a)
_gridState f (GridState st) = GridState <$> f st

-- | Get size of grid in squares
gridSize :: GridState a -> V2 Int
gridSize (GridState v) = V2 (V.length (V.head v)) $ V.length v

-- | A grid of square cells
data Grid a = Grid
  { -- | Color of the lines
    gridColor :: Color,
    -- | Size of each square (edgelength)
    gridSqSize :: Int,
    -- | State of the grid
    gridState :: GridState a
  }
