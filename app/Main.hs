module Main where

import View
import Model
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
  let boardRender boardState = pictures [ drawBoard
                                        , drawBoardState pieceImages boardState ]
  play window background fps initialGameState boardRender (const id) (const id)
