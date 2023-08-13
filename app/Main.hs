{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad (unless)
import Data.Reflection
import Grid
import Grid.Draw
import Life.State
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = AcceleratedRenderer}
  give renderer $ appLoop $ gliderLife $ V2 25 25
  destroyWindow window

appLoop :: (Given Renderer) => LifeState -> IO ()
appLoop state = do
  let renderer = given
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
  drawGrid (Grid (V4 0 0 0 255) 20 state) (V2 0 0)
  present renderer
  delay 50
  unless qPressed (appLoop $ lifeStep state)
