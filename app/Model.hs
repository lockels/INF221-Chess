module Model where

import Data.Array
import Control.Monad.State
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord) 

data PieceColor = White | Black deriving (Eq, Ord)

data Square = Empty | Occupied PieceType PieceColor deriving Eq

type Board = Array (Int, Int) Square

data GameState = GameState 
  { board :: Board
  , currentPlayer :: PieceColor
  , canCastleKingSide :: (Bool, Bool) -- White, Black
  , canCastleQueenSide :: (Bool, Bool) -- White, Black
  , enPassant :: Maybe (Int, Int)
  , selectedSquare :: Maybe (Int, Int)
  }

type Chess a = State GameState a

instance Show PieceType where
  show Pawn = "♙"
  show Knight = "♘"
  show Bishop = "♗"
  show Rook = "♖"
  show Queen = "♕"
  show King = "♔"

instance Show PieceColor where
  show White = "⚪"
  show Black = "⚫"

instance Show Square where
  show Empty = "[]"
  show (Occupied piece color) = show color ++ show piece

initialBoard :: Board
initialBoard = array ((0, 0), (7, 7)) [((i, j), square i j) | i <- [0..7], j <- [0..7]]
  where
    square i j
      | i == 1 = Occupied Pawn White
      | i == 6 = Occupied Pawn Black
      | i == 0 = Occupied (pieceOrder !! j) White
      | i == 7 = Occupied (pieceOrder !! j) Black
      | otherwise = Empty
    pieceOrder = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

initialGameState :: GameState
initialGameState = GameState
  { board = initialBoard
  , currentPlayer = White
  , canCastleKingSide = (True, True)
  , canCastleQueenSide = (True, True)
  , enPassant = Nothing
  , selectedSquare = Nothing
  }

setPiece :: Board -> (Int, Int) -> Square -> Board
setPiece board pos piece = board // [(pos, piece)]

movePiece :: (Int, Int) -> (Int, Int) -> Chess ()
movePiece from to = do
  gameState <- get
  let piece = board gameState ! from
  put $ gameState { board = setPiece (board gameState) to piece }
  put $ gameState { board = setPiece (board gameState) from Empty }

main :: IO ()
main = putStrLn "♔"
