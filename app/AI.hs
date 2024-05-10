module AI where

import Model
import Types
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Array
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import qualified Data.HashMap.Strict as HS

-- type Depth = Int

type Move = (Position, Position)

pieceValue :: PieceType -> Int
pieceValue piece = case piece of
  Pawn   -> 1
  Knight -> 3
  Bishop -> 3
  Rook   -> 5
  Queen  -> 9
  King   -> 1000

-- Evaluate the board by summing up the value of all pieces
evaluateBoard :: GameState -> Int
evaluateBoard gameState =
  sum [ pieceValue (pieceType piece) * colorFactor (pieceColor piece)
      | pos <- range ((0, 0), (7, 7)),
        let square = getPiece (board gameState) pos,
        Occupied piece <- [square] ] -- Filter out Empty squares
  where
    colorFactor color = if color == currentPlayer gameState then 1 else -1

--- Minimax with memoization ---

minimax' :: GameState -> Depth -> Int -> Int -> Bool -> Chess Int
minimax' gameState depth alpha beta isMaximizingPlayer = do
  let boardHash = hashBoard (board gameState)
  let memoKey = (boardHash, depth, isMaximizingPlayer)
  memo <- gets memoTable
  case HS.lookup memoKey memo of
    Just value -> return value
    Nothing -> do
      result <- if depth == 0 || isGameOver gameState
                then return $ evaluateBoard gameState
                else if isMaximizingPlayer
                     then maximumValue gameState depth alpha beta
                     else minimumValue gameState depth alpha beta
      modify (\s -> s { memoTable = HS.insert memoKey result (memoTable s) })
      return result

{-- Minimax without memo, for some reason plays better.
--- assuming the hashing function used for memo
--- causes collisons and therefore faulty evaluation
--} 

minimax :: GameState -> Depth -> Int -> Int -> Bool -> Chess Int
minimax gameState depth alpha beta isMaximizingPlayer
  | depth == 0 || isGameOver gameState = return $ evaluateBoard gameState
  | isMaximizingPlayer = maximumValue gameState depth alpha beta
  | otherwise = minimumValue gameState depth alpha beta

maximumValue :: GameState -> Depth -> Int -> Int -> Chess Int
maximumValue gameState depth alpha beta = do
  moves <- allLegalMoves gameState  
  let sortedMoves = sortBy (comparing $ moveHeuristic gameState) moves
  let alphaOrig = alpha
  foldM (\a move -> do
    let newState = makeMove gameState move
    value <- minimax newState (depth - 1) a beta False
    let newAlpha = max a value
    if newAlpha >= beta then return beta 
    else return newAlpha
    ) alphaOrig sortedMoves

minimumValue :: GameState -> Depth -> Int -> Int -> Chess Int
minimumValue gameState depth alpha beta = do
  moves <- allLegalMoves gameState
  let sortedMoves = sortBy (comparing $ moveHeuristic gameState) moves
  let betaOrig = beta
  foldM (\b move -> do
    let newState = makeMove gameState move
    value <- minimax newState (depth - 1) alpha b True
    let newBeta = min b value
    if newBeta <= alpha then return alpha 
    else return newBeta
    ) betaOrig sortedMoves

-- Placeholder for a function that checks if the game is over
isGameOver :: GameState -> Bool
isGameOver _ = False

-- Function to simulate making a move and returning the new game state
makeMove :: GameState -> Move -> GameState
makeMove gameState (from, to) =
  let newBoard = simulateMove (board gameState) from to
      newState = swapPlayer gameState { board = newBoard }
  in newState

allLegalMoves :: GameState -> Chess [Move]
allLegalMoves gameState = do
  let playerPieces = findPiecesByColor (board gameState) (currentPlayer gameState)
  let legalMoves = concatMap (\from -> map (\to -> (from, to)) (legalMovesForPiece gameState from True)) playerPieces
  return legalMoves

-- Heuristic to evaluate the potential of a move
moveHeuristic :: GameState -> Move -> Int
moveHeuristic gameState (from, to) =
  let fromSquare = getPiece (board gameState) from
      toSquare = getPiece (board gameState) to
      fromValue = case fromSquare of
                    Occupied p -> pieceValue (pieceType p)
                    Empty      -> 0
      toValue = case toSquare of
                  Occupied p -> pieceValue (pieceType p) 
                  Empty      -> 0
      captureBonus = case toSquare of
                       Occupied _ -> 10
                       Empty      -> 0
  in fromValue + toValue + captureBonus

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

-- Infinity placeholder, replace with appropriate value for your implementation
infinity :: Int
infinity = maxBound

-- Update the game loop to use the bestMove function instead of randomMove
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
