{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Word (Word64, Word8)
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = AcceleratedRenderer}
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  drawGrid renderer (Grid (V4 0 0 0 255) (V2 50 50) (V2 10 10)) $ V2 0 0
  present renderer
  delay 50
  unless qPressed (appLoop renderer)

data Grid = Grid
  { -- | Color of the lines
    gridColor :: V4 Word8,
    -- | Number of squares along each edge (x, y)
    gridSquares :: V2 Word64,
    -- | Size of each square (x, y)
    gridSqSize :: V2 Word64
  }

withColor :: (MonadIO m) => Renderer -> V4 Word8 -> (Renderer -> m ()) -> m ()
withColor renderer color f = do
  old <- get $ rendererDrawColor renderer
  rendererDrawColor renderer $= color
  f renderer
  rendererDrawColor renderer $= old

drawGrid :: (MonadIO m) => Renderer -> Grid -> V2 Word64 -> m ()
drawGrid renderer grid pos =
  withColor renderer (gridColor grid) $ \r ->
    mapM_ (uncurry (drawLine r)) $ hlines ++ vlines
  where
    toPoints = map $ P . fmap fromIntegral
    gsq = gridSquares grid
    gsqsz = gridSqSize grid
    hlen = gsqsz ^. _x * gsq ^. _x
    vlen = gsqsz ^. _y * gsq ^. _y
    hlines_s =
      zipWith
        V2
        (replicate (fromIntegral $ 1 + gsq ^. _y) $ pos ^. _x)
        (iterate (+ gsqsz ^. _y) $ pos ^. _y)
    vlines_s =
      zipWith
        V2
        (iterate (+ gsqsz ^. _x) $ pos ^. _x)
        (replicate (fromIntegral $ 1 + gsq ^. _x) $ pos ^. _y)
    hlines = zip (toPoints hlines_s) . toPoints . map (_x +~ hlen) $ hlines_s
    vlines = zip (toPoints vlines_s) . toPoints . map (_y +~ vlen) $ vlines_s

type GridState = V.Vector (V.Vector Bool)

emptyGrid :: V2 Int -> GridState
emptyGrid size = V.replicate (size ^. _y) $ V.replicate (size ^. _x) False

gridStep :: GridState -> GridState
gridStep grid =
  fmap (compcell . neighbours) <$> V.fromList [0..sy - 1]
  where
    sx = length $ V.head grid
    sy = length grid
    v !% i = v ! (i `rem` sx)
    v !%% i = v ! (i `rem` sy)
    neighbours x y =
      [ grid ! x ! y, -- head of this has itself
        grid !%% (y - 1) !% (x - 1),
        grid !%% (y - 1) !% x,
        grid !%% (y - 1) !% (x + 1),
        grid !%% y !% (x - 1),
        grid !%% y !% (x + 1),
        grid !%% (y + 1) !% (x - 1),
        grid !%% (y + 1) !% x,
        grid !%% (y + 1) !% (x + 1)
      ]
    compcell neigh =
      case sum . tail $ neigh of
        2 -> head neigh
        3 -> True
        _ -> False
