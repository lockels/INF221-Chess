module Controller where


import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State

import Model
import View

-- | Handle mouse click events
handleClick :: Event -> GameState -> GameState
handleClick (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState =
  gameState { selectedSquare = Just (screenToSquare (x, y)) }
handleClick _ gameState = gameState

-- | Handle mouse release events
handleRelease :: Event -> GameState -> GameState
handleRelease (EventKey (MouseButton LeftButton) Up _ (x, y)) gameState =
  case selectedSquare gameState of
    Just from -> movePieceIfSelected from (screenToSquare (x, y)) gameState
    Nothing -> gameState
handleRelease _ gameState = gameState

-- | Handle mouse movement events
handleMovement :: Event -> GameState -> GameState
handleMovement (EventMotion (x, y)) gameState =
  case selectedSquare gameState of
    Just _ -> gameState { selectedSquare = Just (screenToSquare (x, y)) }
    Nothing -> gameState
handleMovement _ gameState = gameState

-- | Convert screen coordinates to board coordinates
screenToSquare :: (Float, Float) -> (Int, Int)
screenToSquare (x, y) = (truncate ((x + boardOffset) / squareSize), truncate ((y + boardOffset) / squareSize))

-- | Move piece if a square is selected
movePieceIfSelected :: (Int, Int) -> (Int, Int) -> GameState -> GameState
movePieceIfSelected from to gameState =
  case selectedSquare gameState of
    Just _ -> execState (movePiece from to) gameState { selectedSquare = Nothing }
    Nothing -> gameState

handleEvent :: Event -> GameState -> GameState
handleEvent event gameState = handleRelease event 
                            $ handleMovement event 
                            $ handleClick event gameState
