module AI where

import Model
import Piece
import System.Random (randomRIO)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

pieceValue :: PieceType -> Int
pieceValue piece = case piece of
  Pawn   -> 1
  Knight -> 3
  Bishop -> 3
  Rook   -> 5
  Queen  -> 9
  King   -> 1000 

randomMove :: Chess (Maybe (Position, Position))
randomMove = do
  gameState <- get
  let currentPlayerColor = currentPlayer gameState
  let playerPieces = findPiecesByColor (board gameState) currentPlayerColor
  validMoves <- liftIO $ concatMapM (generateMoves gameState) playerPieces
  case validMoves of
    [] -> return Nothing
    _  -> do
      randomIndex <- liftIO $ randomRIO (0, length validMoves - 1)
      return $ Just (validMoves !! randomIndex)

generateMoves :: GameState -> Position -> IO [(Position, Position)]
generateMoves gameState from = return [(from, to) | to <- legalMovesForPiece gameState from True]

-- Helper function to map over lists within an IO context.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

gameLoop :: Chess ()
gameLoop = do
  gameState <- get
  when (isAITurn gameState) $ do
      move <- randomMove
      case move of
        Just (from, to) -> movePiece from to
        Nothing -> throwError "No moves available for AI"

isAITurn :: GameState -> Bool
isAITurn = (== Black) . currentPlayer
