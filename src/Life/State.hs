module Life.State
  ( LifeState,
    emptyLife,
    gliderLife,
    lifeStep,
  )
where

import Control.Lens (ix)
import Control.Lens.Operators
import Data.Vector ((!))
import qualified Data.Vector as V
import Grid
import Linear.V2 (V2 (..), _x, _y)
import Util

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

-- | A single glider in a game of life board
gliderLife :: V2 Int -> LifeState
gliderLife size =
  emptyLife size
    & _lifeState . ix 0 . ix 1 .~ True
    & _lifeState . ix 1 . ix 2 .~ True
    & _lifeState . ix 2 . ix 0 .~ True
    & _lifeState . ix 2 . ix 1 .~ True
    & _lifeState . ix 2 . ix 2 .~ True

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
