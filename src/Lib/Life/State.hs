module Lib.Life.State
  ( LifeState,
    emptyLife,
    gliderPat,
    lifeStep,
    putPattern,
  )
where

import Control.Lens.At (ix)
import Control.Lens.Indexed (imap)
import Control.Lens.Operators
import Data.Vector ((!))
import qualified Data.Vector as V
import Lib.Grid
import Lib.Util
import Linear.V2 (V2 (..), _x, _y)

type VVBool = V.Vector (V.Vector Bool)

newtype LifeState = LifeState VVBool

-- Lenses for the new type
_lifeState :: Functor f => (VVBool -> f VVBool) -> LifeState -> f LifeState
_lifeState f (LifeState st) = LifeState <$> f st
{-# INLINE _lifeState #-}

instance GridState LifeState where
  gridStateAt (LifeState st) x y = st ! fromIntegral x ! fromIntegral y
  gridSize (LifeState g) = V2 (length $ V.head g) $ length g

-- | Empty Game of Life board
emptyLife :: V2 Int -> LifeState
emptyLife size =
  LifeState (V.replicate (size ^. _y) $ V.replicate (size ^. _x) False)

-- | Put a pattern at a position on a board
putPattern :: [[Bool]] -> V2 Int -> LifeState -> LifeState
putPattern pat (V2 x y) =
  foldr (.) id (concat $ imap (imap . setCell) pat)
  where
    setCell dy dx c = _lifeState . ix (y + dy) . ix (x + dx) .~ c

-- | A single glider
gliderPat :: [[Bool]]
gliderPat = (toBool <$>) <$>
  [ [0, 1, 0],
    [0, 0, 1],
    [1, 1, 1]
  ]

lifeStep :: LifeState -> LifeState
lifeStep (LifeState life) =
  LifeState $ V.fromList $ V.fromList . fmap (compcell . neighbours) <$> genIndices sx sy
  where
    (sx, sy) = (length $ V.head life, length life)
    v !% i = v ! (i `mod` sx)
    v !%% i = v ! (i `mod` sy)
    dy y = (+ y) <$> [0, -1, -1, -1, 0, 0, 1, 1, 1]
    dx x = (+ x) <$> [0, -1, 0, 1, -1, 1, -1, 0, 1]
    -- NOTE: head of this has the cell itself
    neighbours (x, y) = zipWith (!%) ((life !%%) <$> dy y) $ dx x
    compcell neigh =
      case sum . map fromBool . tail $ neigh of
        2 -> head neigh
        3 -> True
        _ -> False
