module Model where 

import Piece
import Control.Monad.Except
import Data.Array
import Control.Monad.State

data Square = Empty | Occupied PieceType PieceColor deriving (Eq, Ord)

type Board = Array Position Square

type Position = (Int, Int) 

data GameState = GameState
  { board :: Board
  , currentPlayer :: PieceColor
  , canCastleKingSide :: (Bool, Bool) -- White, Black
  , canCastleQueenSide :: (Bool, Bool) -- White, Black
  , isCheck :: (Bool, Bool) -- White, Black
  , enPassant :: Maybe Position
  , selectedSquare :: Maybe Position
  , mouseCoordinates :: (Float, Float)
  }

type Chess a = ExceptT String (State GameState) a

runChess :: Chess a -> GameState -> (Either String a, GameState)
runChess chess = runState (runExceptT chess)

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
  , isCheck = (False, False)
  , enPassant = Nothing
  , selectedSquare = Nothing
  , mouseCoordinates = (0, 0)
  }

setPiece :: Board -> Position -> Square -> Board
setPiece chessBoard pos piece = chessBoard // [(pos, piece)]

getPiece :: Board -> Position -> Square
getPiece chessBoard pos = chessBoard ! pos

swapPlayer :: GameState -> GameState
swapPlayer gameState = gameState { currentPlayer = nextPlayer }
  where
    nextPlayer = case currentPlayer gameState of
      White -> Black
      Black -> White

isPlayerTurn :: GameState -> PieceColor -> Bool
isPlayerTurn gameState pieceColor = currentPlayer gameState == pieceColor

isCurrentPlayerPiece :: GameState -> Position -> Bool
isCurrentPlayerPiece gameState pos = case getPiece (board gameState) pos of
  Occupied _ color -> color == currentPlayer gameState
  Empty -> False

movePiece :: Position -> Position -> Chess ()
movePiece from to = do
  gameState <- get
  let piece = getPiece (board gameState) from
  case piece of
    Occupied _ pieceColor -> when (isPlayerTurn gameState pieceColor && isLegalMove gameState from to) $ do
       let updatedBoard  = setPiece (board gameState) to piece
           updatedBoard' = setPiece updatedBoard from Empty
           newState      = swapPlayer gameState { board = updatedBoard', selectedSquare = Nothing }
       put newState
    _ -> return ()

findKingPosition :: GameState -> PieceColor -> Position
findKingPosition gameState color = head [pos | pos <- range ((0, 0), (7, 7)), isKing pos]
  where
    isKing pos = case getPiece (board gameState) pos of
      Occupied King pieceColor -> pieceColor == color
      _ -> False

findPiecesByColor :: GameState -> PieceColor -> [Position]
findPiecesByColor gameState color = [pos | pos <- range ((0, 0), (7, 7)), isPiece pos]
  where
    isPiece pos = case getPiece (board gameState) pos of
      Occupied _ pieceColor -> pieceColor == color
      _ -> False

----- Legal Moves -----

legalMovesForPiece :: GameState -> Position -> [Position]
legalMovesForPiece gameState from
  | not $ isPlayerTurn gameState (currentPlayer gameState) = []
  | not $ isCurrentPlayerPiece gameState from = []
  | otherwise = filter (isLegalMove gameState from) (range ((0, 0), (7, 7)))

isLegalMove :: GameState -> Position -> Position -> Bool
isLegalMove gameState from to
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

collidesWithOwnPiece :: GameState -> Position -> Bool
collidesWithOwnPiece gameState to = case getPiece (board gameState) to of
  Empty -> False
  Occupied _ color -> color == currentPlayer gameState

isPathClear :: Board -> Position -> Position -> Bool
isPathClear chessBoard from to
  | isHorizontalMove from to = isHorizontalPathClear chessBoard from to
  | isVerticalMove from to = isVerticalPathClear chessBoard from to
  | isDiagonalMove from to = isDiagonalPathClear chessBoard from to
  | otherwise = False

isHorizontalPathClear :: Board -> Position -> Position -> Bool
isHorizontalPathClear chessBoard (x, y1) (_, y2) = 
  all (\y -> getPiece chessBoard (x, y) == Empty) [min y1 y2 + 1 .. max y1 y2 - 1]

isVerticalPathClear :: Board -> Position -> Position -> Bool
isVerticalPathClear chessBoard (x1, y) (x2, _) = 
  all (\x -> getPiece chessBoard (x, y) == Empty) [min x1 x2 + 1 .. max x1 x2 - 1]

isDiagonalPathClear :: Board -> Position -> Position -> Bool
isDiagonalPathClear chessBoard (x1, y1) (x2, y2) =
  all (\(x, y) -> getPiece chessBoard (x, y) == Empty) (zip (range x1 x2) (range y1 y2))
  where
    range a b
      | a < b     = [a + 1 .. b - 1]
      | otherwise = reverse [b + 1 .. a - 1]

isCapture :: GameState -> Position -> Bool
isCapture gameState pos = case getPiece (board gameState) pos of
  Occupied _ color -> color /= currentPlayer gameState
  Empty -> False

isLegalPawnMove :: GameState -> Position -> Position -> Bool
isLegalPawnMove gameState (x, y) (x', y') =
  let movingForward = case currentPlayer gameState of -- Standard Move
        White -> x' == x + 1 && y' == y && isSquareEmpty (board gameState) (x', y')
        Black -> x' == x - 1 && y' == y && isSquareEmpty (board gameState) (x', y')
      startingDoubleStep = case currentPlayer gameState of -- Double step at start
        White -> x == 1 && x' == 3 && y' == y 
              && isPathClear (board gameState) (x, y) (x', y')
              && isSquareEmpty (board gameState) (x', y')
        Black -> x == 6 && x' == 4 && y' == y 
              && isPathClear (board gameState) (x, y) (x', y')
              && isSquareEmpty (board gameState) (x', y')
      diagonalCapture = case currentPlayer gameState of  -- Diagonal Capture
        White -> x' == x + 1 && abs (y' - y) == 1 && isCapture gameState (x', y')
        Black -> x' == x - 1 && abs (y' - y) == 1 && isCapture gameState (x', y')
  in movingForward || startingDoubleStep || diagonalCapture

isSquareEmpty :: Board -> Position -> Bool
isSquareEmpty chessBoard pos = getPiece chessBoard pos == Empty

isLegalKnightMove :: GameState -> Position -> Position -> Bool
isLegalKnightMove _ (x, y) (x', y')
  | abs (x' - x) == 2 && abs (y' - y) == 1 = True
  | abs (x' - x) == 1 && abs (y' - y) == 2 = True
  | otherwise = False

isLegalRookMove :: GameState -> Position -> Position -> Bool
isLegalRookMove gameState from to =
  (isHorizontalMove from to || isVerticalMove from to) && isPathClear (board gameState) from to

isLegalBishopMove :: GameState -> Position -> Position -> Bool
isLegalBishopMove gameState from to =
  isDiagonalMove from to && isPathClear (board gameState) from to

isLegalQueenMove :: GameState -> Position -> Position -> Bool
isLegalQueenMove gameState from to =
  (isHorizontalMove from to || isVerticalMove from to || isDiagonalMove from to)
  && isPathClear (board gameState) from to

-- helper functions that define horizontal, vertical, and diagonal moves:

isHorizontalMove :: Eq a => (a, b1) -> (a, b2) -> Bool
isHorizontalMove (x, _) (x', _) = x == x'

isVerticalMove :: Eq a1 => (a2, a1) -> (a3, a1) -> Bool
isVerticalMove (_, y) (_, y') = y == y'

isDiagonalMove :: (Eq a, Num a) => (a, a) -> (a, a) -> Bool
isDiagonalMove (x, y) (x', y') = abs (x' - x) == abs (y' - y)

isLegalKingMove :: GameState -> Position -> Position -> Bool
isLegalKingMove gameState (x, y) (x', y')
  | abs (x' - x) <= 1 && abs (y' - y) <= 1 = True
  | otherwise = False

isInCeck :: GameState -> PieceColor -> Bool
isInCeck gameState color = any (canCaptereKing gameState color) (findPiecesByColor gameState (oppositeColor color))

-- helper functions that define check rules:

canCaptereKing :: GameState -> PieceColor -> Position -> Bool
canCaptereKing gameState color pos = case getPiece (board gameState) pos of
  Occupied _ pieceColor ->
    pieceColor == color &&
    isLegalMove gameState pos (findKingPosition gameState (oppositeColor color))
  _ -> False

isCheckPosition :: GameState -> PieceColor -> Position -> Bool
isCheckPosition gameState color pos = case getPiece (board gameState) pos of
  Occupied _ pieceColor -> pieceColor == color && canCaptereKing gameState color pos
  _ -> False

main :: IO ()
main = putStrLn "♔"
