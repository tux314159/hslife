module Lib.Life.State
  ( LifeState,
    emptyLife,
    gliderPat,
    gosperPat,
    lifeStep,
    putPattern,
  )
where

import Control.Lens.At (ix)
import Control.Lens.Indexed (imap)
import Control.Lens.Operators
import Data.List.Split (splitOn)
import Data.Vector ((!))
import qualified Data.Vector as V
import Lib.Grid
import Lib.Util
import Linear.V2 (V2 (..), _x, _y)

type LifeState = GridState Bool

-- | Empty Game of Life board
emptyLife :: V2 Int -> LifeState
emptyLife size =
  GridState (V.replicate (size ^. _y) $ V.replicate (size ^. _x) False)

-- | Put a pattern at a position on a board
putPattern :: [[Bool]] -> V2 Int -> LifeState -> LifeState
putPattern pat (V2 x y) =
  foldr (.) id (concat $ imap (imap . setCell) pat)
  where
    setCell dy dx c = _gridState . ix (y + dy) . ix (x + dx) .~ c


-- | Convert a string of '-' 'X' '\n' into a pattern
toPat :: String -> [[Bool]]
toPat s = map (== 'X') <$> splitOn "\n" s

-- | A single glider
gliderPat :: [[Bool]]
gliderPat = toPat
  "-X-\n\
  \--X\n\
  \XXX"

-- | Gosper glider gun
gosperPat :: [[Bool]]
gosperPat = toPat
  "--------------------------X-----------\n\
  \------------------------X-X-----------\n\
  \--------------XX------XX------------XX\n\
  \-------------X---X----XX------------XX\n\
  \--XX--------X-----X---XX--------------\n\
  \--XX--------X---X-XX----X-X-----------\n\
  \------------X-----X-------X-----------\n\
  \-------------X---X--------------------\n\
  \--------------XX----------------------"
  

lifeStep :: LifeState -> LifeState
lifeStep (GridState life) =
  GridState $ V.fromList $ V.fromList . fmap (compcell . neighbours) <$> genIndices sx sy
  where
    (sx, sy) = (length $ V.head life, length life)
    v !~ i = v ! (i `mod` sx)
    v !~~ i = v ! (i `mod` sy)
    dy y = (+ y) <$> [0, -1, -1, -1, 0, 0, 1, 1, 1]
    dx x = (+ x) <$> [0, -1, 0, 1, -1, 1, -1, 0, 1]
    -- NOTE: head of this has the cell itself
    neighbours (x, y) = zipWith (!~) ((life !~~) <$> dy y) $ dx x
    compcell neigh =
      case sum . map fromBool . tail $ neigh of
        2 -> head neigh
        3 -> True
        _ -> False
