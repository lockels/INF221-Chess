module Main where

import View
import Types
import Controller
import AI (gameLoop, isAITurn)

import Graphics.Gloss.Interface.IO.Game (playIO, Event)
import Graphics.Gloss
import Control.Monad (when, void)
import Control.Concurrent (forkIO, threadDelay, MVar) 
import Control.Concurrent.MVar (readMVar, newMVar, takeMVar, putMVar)
import Control.Exception (catch, SomeException)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    pieceImages <- loadPieceImages
    let initialState = initialGameState
    gameStateVar <- newMVar initialState
    playIO window background fps (pieceImages, gameStateVar) renderFunction handleEventIO (const return)

background :: Color
background = black 

fps :: Int
fps = 60

window :: Display
window = InWindow "INF221 Chess" boardSize (10 , 10)

renderFunction :: (PieceImages, MVar GameState) -> IO Picture
renderFunction (pieceImages, gameStateVar) = do
    gameState <- readMVar gameStateVar
    boardRender pieceImages gameState

handleEventIO :: Event -> (PieceImages, MVar GameState) -> IO (PieceImages, MVar GameState)
handleEventIO event (pieceImages, gameStateVar) = do
    gameState <- takeMVar gameStateVar
    (result, newState) <- runChess (handleClick event >> handleRelease event) gameState
    case result of
        Right _ -> do
            putMVar gameStateVar newState
            when (isAITurn newState) $ void $ forkIO $ aiMove gameStateVar
            return (pieceImages, gameStateVar)
        Left errMsg -> do
            putStrLn $ "Player action error: " ++ errMsg
            putMVar gameStateVar gameState
            return (pieceImages, gameStateVar)

aiMove :: MVar GameState -> IO ()
aiMove gameStateVar = do
  -- thread delay so that the interface is snappier
  threadDelay 1000
  gameState <- takeMVar gameStateVar
  catch (do
    (aiResult, aiState) <- runChess gameLoop gameState
    case aiResult of
      Right _ -> do
        putMVar gameStateVar aiState
      Left errMsg -> do
        hPutStrLn stderr $ "AI action error: " ++ errMsg
        putMVar gameStateVar gameState
    ) (\e -> do
        hPutStrLn stderr $ "Caught exception: " ++ show (e :: SomeException)
        putMVar gameStateVar gameState
      )

boardRender :: PieceImages -> GameState -> IO Picture
boardRender pieceImages gameState = return $ pictures
    [ drawBoard
    , drawBoardState pieceImages gameState
    , drawLegalMovesForPiece gameState (selectedSquare gameState)
    ]

