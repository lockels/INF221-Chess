module Piece where
import Data.Hashable

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
