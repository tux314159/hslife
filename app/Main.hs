{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Reflection
import Lib.Grid
import Lib.Grid.Draw
import Lib.Life.State
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = AcceleratedRenderer}
  give renderer $ appLoop 1 $ putPattern gliderPat (V2 0 0) (emptyLife $ V2 25 25)
  destroyWindow window

appLoop :: (Given Renderer) => Int -> LifeState -> IO ()
appLoop frame state = do
  let renderer = given
  startTime <- ticks
  -- Handle events
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  -- Step
  let state' = if frame == 0 then lifeStep state else state
  -- Background
  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  -- Draw
  drawGrid (Grid (V4 0 0 0 255) 20 state') (V2 0 0)
  -- Render
  present renderer
  -- We want 30fps
  endTime <- ticks
  delay (33 - (endTime - startTime))
  unless qPressed (appLoop (succ frame `rem` 2) state')
