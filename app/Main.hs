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
  window <- createWindow "My SDL Application" $ defaultWindow {windowInitialSize = V2 900 900}
  renderer <- createRenderer window (-1) defaultRenderer {rendererType = SoftwareRenderer}
  _ <- execStateT (give renderer appLoop) initialState
  destroyWindow window
  where
    initialGrid =
      putPattern gosperPat (V2 10 0) $
        putPattern (reverse $ reverse <$> gosperPat) (V2 0 60) $
          emptyLife $ V2 75 75
    initialState = AppState 1 initialGrid

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
  give renderer $ drawGrid (Grid (V4 0 0 0 255) 10 state') (V2 0 0)
  -- Render
  present renderer
  -- We want 60fps
  endTime <- ticks
  delay $ max 0 (17 - (endTime - startTime))

  put $ AppState (succ frame `rem` 2) state'
  unless qPressed appLoop
