module View where

import Graphics.Gloss
import Model
import Data.Array
import qualified Data.Map as Map

squareSize :: Float
squareSize = 75.0

boardSize :: (Int, Int)
boardSize = (round $ 8 * squareSize, round $ 8 * squareSize)

boardOffset :: Float
boardOffset = 3.5 * squareSize

lightSquareColor :: Color
lightSquareColor = makeColorI 233 174 95 255 

darkSquareColor :: Color
darkSquareColor = makeColorI 142 71 0 255 

drawSquare :: Color -> (Float, Float) -> Picture
drawSquare c (x, y) = translate x y $ color c $ rectangleSolid squareSize squareSize

drawBoard :: Picture
drawBoard = pictures [ drawSquare (if even (i + j) then darkSquareColor else lightSquareColor)
                      (squareSize * fromIntegral i - boardOffset,
                       squareSize * fromIntegral j - boardOffset)
                      | i <- [0..7] :: [Int], j <- [0..7] :: [Int] ]

type PieceImages = Map.Map (PieceType, PieceColor) Picture

loadPieceImages :: IO PieceImages
loadPieceImages = do 
  whitePawn   <- loadBMP "images/white-pawn.bmp"
  blackPawn   <- loadBMP "images/black-pawn.bmp"
  whiteKnight <- loadBMP "images/white-knight.bmp"
  blackKnight <- loadBMP "images/black-knight.bmp"
  whiteBishop <- loadBMP "images/white-bishop.bmp"
  blackBishop <- loadBMP "images/black-bishop.bmp"
  whiteRook   <- loadBMP "images/white-rook.bmp"
  blackRook   <- loadBMP "images/black-rook.bmp"
  whiteQueen  <- loadBMP "images/white-queen.bmp"
  blackQueen  <- loadBMP "images/black-queen.bmp"
  whiteKing   <- loadBMP "images/white-king.bmp"
  blackKing   <- loadBMP "images/black-king.bmp"
  return $ Map.fromList [((Pawn, White), whitePawn), ((Pawn, Black), blackPawn),
                         ((Knight, White), whiteKnight), ((Knight, Black), blackKnight),
                         ((Bishop, White), whiteBishop), ((Bishop, Black), blackBishop),
                         ((Rook, White), whiteRook), ((Rook, Black), blackRook),
                         ((Queen, White), whiteQueen), ((Queen, Black), blackQueen),
                         ((King, White), whiteKing), ((King, Black), blackKing)
                         ]

drawSquareContents :: PieceImages -> Square -> (Int, Int) -> Picture
drawSquareContents images square (i, j) = 
  case square of
    Occupied piece color 
      -> translate 
         (fromIntegral j * squareSize - boardOffset) 
         (fromIntegral i * squareSize - boardOffset) 
         $ images Map.! (piece, color)
    Empty 
      -> blank

drawBoardState :: PieceImages -> GameState -> Picture
drawBoardState images gameState = pictures 
  [ drawSquareContents images square (i, j) 
  | ((i, j), square) <- assocs (board gameState) ]
