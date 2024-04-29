module Main where

import View
import Model
import Controller
import Graphics.Gloss.Interface.IO.Game (playIO, Event)
import Graphics.Gloss
import AI (gameLoop)

background :: Color
background = black 

fps :: Int
fps = 60

window :: Display
window = InWindow "INF221 Chess" boardSize (10 , 10)

handleEventIO :: Event -> GameState -> IO GameState
handleEventIO event gameState = do
  (result, newState) <- runChess (handleClick event >> handleRelease event >> handleMotion event) gameState
  case result of
    Right _ -> do
      -- After handling the player's event, run the game loop to check for AI's turn
      (aiResult, aiState) <- runChess gameLoop newState
      case aiResult of
        Right _ -> return aiState
        Left errMsg -> do
          putStrLn $ "AI error: " ++ errMsg
          return newState
    Left errMsg -> do
      putStrLn $ "Error: " ++ errMsg
      return newState

main :: IO ()
main = do
  pieceImages <- loadPieceImages
  let boardRender gameState = return $ pictures [ drawBoard
                                                , drawBoardState pieceImages gameState
                                                , drawLegalMovesForPiece gameState (selectedSquare gameState)
                                                , renderCoordinateText (mouseCoordinates gameState)]
  playIO window background fps initialGameState boardRender handleEventIO (const return)
