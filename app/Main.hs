{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = AcceleratedRenderer}
  appLoop renderer $ gliderGrid $ V2 25 25
  destroyWindow window

type Color = V4 Word8

appLoop :: Renderer -> GridState -> IO ()
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
  unless qPressed (appLoop renderer $ gridStep state)

-- | Generate 2d list with (col-major!) indices
genIndices :: Integral a => a -> a -> [[(a, a)]]
genIndices x y = zipWith zip (replicate (fromIntegral y) [0 .. x - 1]) (replicate (fromIntegral x) <$> [0 .. y - 1])

type GridState = V.Vector (V.Vector Bool)

emptyGrid :: V2 Int -> GridState
emptyGrid size = V.replicate (size ^. _y) $ V.replicate (size ^. _x) False

gliderGrid :: V2 Int -> GridState
gliderGrid size =
  emptyGrid size
    & ix 0 . ix 1 .~ True
    & ix 1 . ix 2 .~ True
    & ix 2 . ix 0 .~ True
    & ix 2 . ix 1 .~ True
    & ix 2 . ix 2 .~ True

fromBool :: Bool -> Int
fromBool False = 0
fromBool True = 1

gridStep :: GridState -> GridState
gridStep grid =
  V.fromList $
    V.fromList . fmap (compcell . neighbours)
      <$> genIndices sx sy
  where
    sx = length $ V.head grid
    sy = length grid
    v !% i = v ! (i `mod` sx)
    v !%% i = v ! (i `mod` sy)
    neighbours (x, y) =
      [ grid ! y ! x, -- head of this has itself
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
      case sum . map fromBool . tail $ neigh of
        2 -> head neigh
        3 -> True
        _ -> False

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

drawGrid :: (MonadIO m) => Renderer -> Grid -> GridState -> V2 CInt -> m ()
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
