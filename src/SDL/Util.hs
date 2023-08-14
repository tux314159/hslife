module SDL.Util
  ( Color,
    withColor,
    LineRect (..),
    drawRectLine,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL

type Color = V4 Word8

-- | Perform some operation with a color, then restore after
withColor :: (MonadIO m) => Renderer -> Color -> (Renderer -> m ()) -> m ()
withColor renderer color f = do
  old <- get $ rendererDrawColor renderer
  rendererDrawColor renderer $= color
  f renderer
  rendererDrawColor renderer $= old

-- | Line{horiz/vert}, starting from a point with some length,
-- drawn with a rectangle of thickness 1
data LineRect = LineH (Point V2 CInt) CInt | LineV (Point V2 CInt) CInt

drawRectLine :: (MonadIO m) => Renderer -> LineRect -> m ()
drawRectLine r (LineH start len) = fillRect r $ Just $ Rectangle start (V2 len 1)
drawRectLine r (LineV start len) = fillRect r $ Just $ Rectangle start (V2 1 len)
