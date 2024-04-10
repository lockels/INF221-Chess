module Controller where

import Control.Monad.State
import Graphics.Gloss
import Model
import Graphics.Gloss.Interface.Pure.Game

screenToBoard :: (Float, Float) -> (Int, Int)
screenToBoard (x, y) = (locate y, locate x)

locate :: Float -> Int
locate x 
  | x > -300 && x < -225 = 0 
  | x > -225 && x < -150 = 1
  | x > -150 && x < -75 = 2 
  | x > -75 && x < 0 = 3 
  | x > 0 && x < 75 = 4 
  | x > 75 && x < 150 = 5
  | x > 150 && x < 225 = 6 
  | x > 225 && x < 300 = 7
  | otherwise = -1

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
handleEvent event gameState = case runChess (handleClick event >> handleRelease event >> handleMotion event) gameState of
  (Right _, newState) -> newState
  (Left errMsg, newState) -> newState

mouseCoordinatesToString :: (Float, Float) -> String
mouseCoordinatesToString (x, y) = "(" ++ show (round x :: Int) ++ ", "
                                      ++ show (round y :: Int) ++ ")"

renderCoordinateText :: (Float, Float) -> Picture
renderCoordinateText (x, y) =
  let coordinates = mouseCoordinatesToString (x, y)
      translatedX = x + 10
      translatedY = y + 10
    in translate translatedX translatedY $ scale 0.1 0.1 $ color black $ text coordinates
