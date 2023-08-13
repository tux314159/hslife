{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Life.State
import SDL
import UI.Grid (Grid (..), drawGrid)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = AcceleratedRenderer}
  appLoop renderer $ gliderLife $ V2 25 25
  destroyWindow window

appLoop :: Renderer -> LifeState -> IO ()
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
  unless qPressed (appLoop renderer $ lifeStep state)
