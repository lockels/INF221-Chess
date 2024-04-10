module Piece where

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord)

data PieceColor = White | Black deriving (Eq, Ord)

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
