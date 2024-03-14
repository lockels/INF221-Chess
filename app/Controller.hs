module Controller where

import Control.Monad.State
import Graphics.Gloss
import Model
import Graphics.Gloss.Interface.Pure.Game
import View

screenToBoard :: (Float, Float) -> (Int, Int)
screenToBoard (x, y) =
  let boardX = round $ (x + boardOffset) / squareSize
      boardY = round $ -(y + boardOffset) / squareSize
  in if isOnBoard boardX && isOnBoard boardY
      then (boardX, boardY)
      else (-1, -1)

isOnBoard :: Int -> Bool
isOnBoard pos = pos >= 0 && pos <= 7 

handleClick :: Event -> Chess ()
handleClick (EventKey (MouseButton LeftButton) Down _ (x, y)) = do
  gameState <- get
  let boardPos = screenToBoard (x, y)
  put $ gameState { selectedSquare = Just boardPos }
handleClick _ = return ()

handleRelease :: Event -> Chess ()
handleRelease (EventKey (MouseButton LeftButton) Up _ (x, y)) = do
  gameState <- get
  case selectedSquare gameState of
    Just from -> do
      let to = screenToBoard (x, y)
      movePiece from to
    Nothing -> return ()
handleRelease _ = return ()

handleMotion :: Event ->  Chess ()
handleMotion (EventMotion (x, y)) = do
  gameState <- get
  put $ gameState { mouseCoordinates = (x, y) }
handleMotion _ = return ()

handleEvent :: Event -> GameState -> GameState
handleEvent event = execState 
                  $ handleClick event 
                 >> handleRelease event 
                 >> handleMotion event

mouseCoordinatesToString :: (Float, Float) -> String
mouseCoordinatesToString (x, y) = "(" ++ show (round x) ++ ", " ++ show (round y) ++ ")"

renderCoordinateText :: (Float, Float) -> Picture
renderCoordinateText (x, y) =
  let coordinates = mouseCoordinatesToString (x, y)
      translatedX = x + 10
      translatedY = y + 10
    in translate translatedX translatedY $ scale 0.1 0.1 $ color black $ text coordinates
