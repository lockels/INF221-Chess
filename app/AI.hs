module AI where

import Model
import System.Random (randomRIO)
import Control.Monad.State
import Control.Monad.Except 
import Control.Monad.IO.Class 

-- instance MonadIO (ExceptT String (State GameState)) where
--   liftIO = lift . liftIO
-- 
-- randomMove :: Chess (Maybe (Position, Position))
-- randomMove = do
--   gameState <- get
--   let currentPlayerColor = currentPlayer gameState
--   let playerPieces = findPiecesByColor gameState currentPlayerColor
--   validMoves <- liftIO $ concatMapM (generateMoves gameState) playerPieces
--   if null validMoves then
--     return Nothing
--   else do
--     randomIndex <- liftIO $ randomRIO (0, length validMoves - 1)
--     return $ Just (validMoves !! randomIndex)
-- 
-- generateMoves :: GameState -> Position -> IO [(Position, Position)]
-- generateMoves gameState from = return [(from, to) | to <- legalMovesForPiece gameState from]
-- 
-- -- Helper function to map over lists within an IO context.
-- concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
-- concatMapM f xs = concat <$> mapM f xs
