module SDL.Util where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word8)
import SDL

type Color = V4 Word8

withColor :: (MonadIO m) => Renderer -> Color -> (Renderer -> m ()) -> m ()
withColor renderer color f = do
  old <- get $ rendererDrawColor renderer
  rendererDrawColor renderer $= color
  f renderer
  rendererDrawColor renderer $= old

