module AI where

import Model
import Piece
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

type Depth = Int

type Move = (Position, Position)

pieceValue :: PieceType -> Int
pieceValue piece = case piece of
  Pawn   -> 1
  Knight -> 3
  Bishop -> 3
  Rook   -> 5
  Queen  -> 9
  King   -> 1000

evaluateBoard :: GameState -> Int
evaluateBoard gameState =
  sum [ pieceValue (pieceType piece) * colorFactor (pieceColor piece)
      | pos <- range ((0, 0), (7, 7)),
        let square = getPiece (board gameState) pos,
        Occupied piece <- [square] ] -- Filter out empty spots
  where
    colorFactor color = if color == currentPlayer gameState then 1 else -1

minimax :: GameState -> Depth -> Int -> Int -> Bool -> Chess Int
minimax gameState depth alpha beta isMaximizingPlayer
  | depth == 0 || isGameOver gameState = return $ evaluateBoard gameState
  | isMaximizingPlayer = maximumValue gameState depth alpha beta
  | otherwise = minimumValue gameState depth alpha beta

maximumValue :: GameState -> Depth -> Int -> Int -> Chess Int
maximumValue gameState depth alpha beta = do
  moves <- allLegalMoves gameState
  let alphaOrig = alpha
  foldM (\a move -> do
    let newState = makeMove gameState move
    value <- minimax newState (depth - 1) a beta False
    let newAlpha = max a value
    if newAlpha >= beta then -- Prune the branch
      return beta
    else
      return newAlpha
    ) alphaOrig moves

minimumValue :: GameState -> Depth -> Int -> Int -> Chess Int
minimumValue gameState depth alpha beta = do
  moves <- allLegalMoves gameState
  let betaOrig = beta
  foldM (\b move -> do
    let newState = makeMove gameState move
    value <- minimax newState (depth - 1) alpha b True
    let newBeta = min b value
    if newBeta <= alpha then -- Prune the branch
      return alpha
    else
      return newBeta
    ) betaOrig moves


-- Placeholder for a function that checks if the game is over
isGameOver :: GameState -> Bool
isGameOver gameState = False

makeMove :: GameState -> Move -> GameState
makeMove gameState (from, to) =
  let newBoard = simulateMove (board gameState) from to
      newState = swapPlayer gameState { board = newBoard }
  in newState

allLegalMoves :: GameState -> Chess [Move]
allLegalMoves gameState = do
  let playerPieces = findPiecesByColor (board gameState) (currentPlayer gameState)
  let legalMoves = concatMap (\from -> map (\to -> (from, to))
                             (legalMovesForPiece gameState from True)) playerPieces
  return legalMoves

bestMove :: Chess (Maybe Move)
bestMove = do
  gameState <- get
  let depth = 3
  let isMaximizingPlayer = currentPlayer gameState == Black
  moves <- allLegalMoves gameState
  scoredMoves <- forM moves $ \move -> do
    let newState = makeMove gameState move
    score <- minimax newState depth (-infinity) infinity (not isMaximizingPlayer)
    return (score, move)
  case scoredMoves of
    [] -> return Nothing
    _ -> return $ Just $ snd $ maximumBy (comparing fst) scoredMoves

infinity :: Int
infinity = maxBound

gameLoop :: Chess ()
gameLoop = do
  gameState <- get
  when (isAITurn gameState) $ do
      move <- bestMove
      case move of
        Just (from, to) -> movePiece from to
        Nothing -> throwError "No moves available for AI"

isAITurn :: GameState -> Bool
isAITurn = (== Black) . currentPlayer
