{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (execStateT, get, put)
import Data.Reflection
import Lib.Grid
import Lib.Grid.Draw
import Lib.Life.State
import SDL hiding (get)

data AppState = AppState
  { appFrame :: Int,
    appLifeState :: LifeState
  }

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = AcceleratedRenderer}
  let initialState = AppState 1 $ putPattern gliderPat (V2 0 0) (emptyLife $ V2 25 25)
  _ <- execStateT (give renderer appLoop) initialState
  destroyWindow window

appLoop :: (Given Renderer, MonadIO m, MonadState AppState m) => m ()
appLoop = do
  let renderer = given
  frame <- appFrame <$> get
  state <- appLifeState <$> get

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
  give renderer $ drawGrid (Grid (V4 0 0 0 255) 20 state') (V2 0 0)
  -- Render
  present renderer
  -- We want 30fps
  endTime <- ticks
  delay (33 - (endTime - startTime))

  put $ AppState (succ frame `rem` 2) state'
  unless qPressed appLoop
