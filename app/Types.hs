module Types where

import Control.Monad.Except
import Data.Array
import Data.List (foldl')
import Control.Monad.State
import Data.Hashable
import qualified Data.HashMap.Strict as HS
import Data.Bits (xor)

data Piece = Piece {
  pieceColor :: PieceColor,
  pieceType :: PieceType
  } deriving (Eq, Ord)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord)

data PieceColor = White | Black deriving (Eq, Ord)

instance Hashable PieceType where
  hashWithSalt salt Pawn   = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Knight = hashWithSalt salt (1 :: Int)
  hashWithSalt salt Bishop = hashWithSalt salt (2 :: Int)
  hashWithSalt salt Rook   = hashWithSalt salt (3 :: Int)
  hashWithSalt salt Queen  = hashWithSalt salt (4 :: Int)
  hashWithSalt salt King   = hashWithSalt salt (5 :: Int)

instance Hashable PieceColor where
  hashWithSalt salt White = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Black = hashWithSalt salt (1 :: Int)

instance Hashable Piece where
    hashWithSalt salt (Piece color ptype) = hashWithSalt salt (color, ptype)

oppositeColor :: PieceColor -> PieceColor
oppositeColor White = Black
oppositeColor Black = White

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

data Square = Empty | Occupied Piece deriving (Eq, Ord)

type Board = Array Position Square

type Position = (Int, Int)

type Depth = Int
type MemoKey = (Int, Depth, Bool)
type Memo = HS.HashMap MemoKey Int

data GameState = GameState
  { board :: Board
  , currentPlayer :: PieceColor
  , canCastleKingSide :: (Bool, Bool) -- White, Black
  , canCastleQueenSide :: (Bool, Bool) -- White, Black
  , isCheck :: (Bool, Bool) -- White, Black
  , enPassant :: Maybe Position
  , selectedSquare :: Maybe Position
  , memoTable :: Memo
  }

instance Show GameState where
    show gs = "GameState {currentPlayer = " ++ show (currentPlayer gs)
          ++  ", canCastleKingSide = " ++ show (canCastleKingSide gs)
          ++  ", canCastleQueenSide = " ++ show (canCastleQueenSide gs)
          ++  ", isCheck = " ++ show (canCastleQueenSide gs)
          ++  ", enPassant = " ++ show (enPassant gs)
          ++  ", selectedSquare = " ++ show (selectedSquare gs)

instance Eq GameState where
    gs1 == gs2 = board gs1 == board gs2
           && currentPlayer gs1 == currentPlayer gs2
           && canCastleKingSide gs1 == canCastleKingSide gs2
           && canCastleQueenSide gs1 == canCastleQueenSide gs2
           && isCheck gs1 == isCheck gs2
           && enPassant gs1 == enPassant gs2
           && selectedSquare gs1 == selectedSquare gs2

instance Hashable Square where
    hashWithSalt salt Empty = hashWithSalt salt (0 :: Int)
    hashWithSalt salt (Occupied piece) = hashWithSalt salt piece

hashBoard :: Board -> Int
hashBoard b = foldl' (\acc idx -> acc `xor` hashWithSalt acc (b ! idx)) 0 (indices b)

instance Hashable GameState where
    hashWithSalt salt gs = hashWithSalt salt (hashBoard (board gs), currentPlayer gs)

type Chess a = ExceptT String (StateT GameState IO) a

runChess :: Chess a -> GameState -> IO (Either String a, GameState)
runChess chess = runStateT (runExceptT chess)

instance Show Square where
  show Empty = "[]"
  show (Occupied piece) = show (pieceColor piece) ++ show (pieceType piece)

initialBoard :: Board
initialBoard = array ((0, 0), (7, 7)) [((i, j), square i j) | i <- [0..7], j <- [0..7]]
  where
    square i j
      | i == 1 = Occupied (Piece White Pawn)
      | i == 6 = Occupied (Piece Black Pawn)
      | i == 0 = Occupied (Piece White (pieceOrder !! j))
      | i == 7 = Occupied (Piece Black (pieceOrder !! j))
      | otherwise = Empty
    pieceOrder = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

initialGameState :: GameState
initialGameState = GameState
  { board = initialBoard
  , currentPlayer = White
  , canCastleKingSide = (True, True)
  , canCastleQueenSide = (True, True)
  , isCheck = (False, False)
  , enPassant = Nothing
  , selectedSquare = Nothing
  , memoTable = HS.empty
  }
