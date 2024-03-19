module Model where

import Data.Array
import Control.Monad.State

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord) 

data PieceColor = White | Black deriving (Eq, Ord)

data Square = Empty | Occupied PieceType PieceColor 

type Board = Array (Int, Int) Square

data GameState = GameState 
  { board :: Board
  , currentPlayer :: PieceColor
  , canCastleKingSide :: (Bool, Bool) -- White, Black
  , canCastleQueenSide :: (Bool, Bool) -- White, Black
  , enPassant :: Maybe (Int, Int)
  , selectedSquare :: Maybe (Int, Int)
  , mouseCoordinates :: (Float, Float)
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
  , mouseCoordinates = (0, 0)
  }

setPiece :: Board -> (Int, Int) -> Square -> Board
setPiece chessBoard pos piece = chessBoard // [(pos, piece)]

getPiece :: Board -> (Int, Int) -> Square
getPiece chessBoard pos = chessBoard ! pos

isLegalMove :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isLegalMove gameState from to 
  | from == to = False
  | collidesWithOwnPiece gameState to = False
  | otherwise = case getPiece (board gameState) from of
      Empty -> False
      Occupied piece _ -> case piece of
        Pawn   -> isLegalPawnMove gameState from to
        Knight -> isLegalKnightMove gameState from to
        Bishop -> isLegalBishopMove gameState from to
        Rook   -> isLegalRookMove gameState from to
        Queen  -> isLegalQueenMove gameState from to
        King   -> isLegalKingMove gameState from to

collidesWithOwnPiece :: GameState -> (Int, Int) -> Bool
collidesWithOwnPiece gameState to = case getPiece (board gameState) to of
  Empty -> False
  Occupied _ color -> color == currentPlayer gameState

isLegalPawnMove :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isLegalPawnMove gameState (x, y) (x', y')
  | x' == x + 1 && y' == y = True
  | x' == x + 2 && y' == y = True
  | otherwise = False

isLegalKnightMove :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isLegalKnightMove gameState (x, y) (x', y')
  | abs (x' - x) == 2 && abs (y' - y) == 1 = True
  | abs (x' - x) == 1 && abs (y' - y) == 2 = True
  | otherwise = False

isLegalBishopMove :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isLegalBishopMove gameState (x, y) (x', y')
  | abs (x' - x) == abs (y' - y) = True
  | otherwise = False

isLegalRookMove :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isLegalRookMove gameState (x, y) (x', y')
  | x' == x || y' == y = True
  | otherwise = False

isLegalQueenMove :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isLegalQueenMove gameState (x, y) (x', y')
  | isLegalBishopMove gameState (x, y) (x', y') = True
  | isLegalRookMove gameState (x, y) (x', y') = True
  | otherwise = False

isLegalKingMove :: GameState -> (Int, Int) -> (Int, Int) -> Bool
isLegalKingMove gameState (x, y) (x', y')
  | abs (x' - x) <= 1 && abs (y' - y) <= 1 = True
  | otherwise = False

movePiece :: (Int, Int) -> (Int, Int) -> Chess ()
movePiece from to = do
  gameState <- get
  if isLegalMove gameState from to
   then do
     let piece = board gameState ! from
         updatedBoard  = setPiece (board gameState) to piece
         updatedBoard' = setPiece updatedBoard from Empty
     put gameState { board = updatedBoard' , selectedSquare = Nothing }
   else return ()

main :: IO ()
main = putStrLn "♔"
