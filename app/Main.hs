module Main where

import View
import Model
import Controller

import Graphics.Gloss

background :: Color
background = white

fps :: Int
fps = 60

window :: Display
window = InWindow "INF221 Chess" boardSize (10 , 10)

main :: IO ()
main = do
  pieceImages <- loadPieceImages
  let boardRender gameState = pictures [ drawBoard
                                       , drawBoardState pieceImages gameState
                                       , renderCoordinateText (mouseCoordinates gameState)]
  play window background fps initialGameState boardRender handleEvent (const id)
