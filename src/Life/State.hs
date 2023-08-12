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
import Linear.V2 (V2, _x, _y)
import Util (fromBool, genIndices)

type LifeState = V.Vector (V.Vector Bool)

-- | Empty Game of Life board
emptyLife :: V2 Int -> LifeState
emptyLife size = V.replicate (size ^. _y) $ V.replicate (size ^. _x) False

-- | A single glider in a game of life board
gliderLife :: V2 Int -> LifeState
gliderLife size =
  emptyLife size
    & ix 0 . ix 1 .~ True
    & ix 1 . ix 2 .~ True
    & ix 2 . ix 0 .~ True
    & ix 2 . ix 1 .~ True
    & ix 2 . ix 2 .~ True

lifeStep :: LifeState -> LifeState
lifeStep life =
  V.fromList $
    V.fromList . fmap (compcell . neighbours)
      <$> genIndices sx sy
  where
    sx = length $ V.head life
    sy = length life
    v !% i = v ! (i `mod` sx)
    v !%% i = v ! (i `mod` sy)
    neighbours (x, y) =
      [ life ! y ! x, -- head of this has itself
        life !%% (y - 1) !% (x - 1),
        life !%% (y - 1) !% x,
        life !%% (y - 1) !% (x + 1),
        life !%% y !% (x - 1),
        life !%% y !% (x + 1),
        life !%% (y + 1) !% (x - 1),
        life !%% (y + 1) !% x,
        life !%% (y + 1) !% (x + 1)
      ]
    compcell neigh =
      case sum . map fromBool . tail $ neigh of
        2 -> head neigh
        3 -> True
        _ -> False
