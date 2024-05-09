{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.QuickCheck
import Model
import Piece  
import Data.Array
import Control.Monad
import Control.Monad.State
import Control.Monad.Except (runExceptT)
import GHC.TypeError (ErrorMessage(Text))

main :: IO ()
main = do
  putStrLn "Testing swapPlayer..."
  quickCheck prop_swapPlayerTwice
  putStrLn "Testing isLegalPawnMove..."
  quickCheck prop_legalPawnMoves
  putStrLn "Testing isLegalKnightMove..."
  quickCheck prop_legalKnightMoves
  
instance Arbitrary PieceColor where
  arbitrary = elements [White, Black]

instance Arbitrary PieceType where
  arbitrary = elements [Pawn, Knight, Bishop, Rook, Queen, King]

instance Arbitrary Piece where
  arbitrary  = Piece <$> arbitrary <*> arbitrary

instance Arbitrary Square where
    arbitrary = frequency [(1, return Empty), (9, Occupied <$> arbitrary)]

instance Arbitrary Board where
    arbitrary = array ((0, 0), (7, 7)) <$> vectorOf 64 arbitrary

instance Arbitrary GameState where
  arbitrary = do
    brd         <- arbitrary 
    currentP    <- arbitrary
    castleK     <- arbitraryBoolPair
    castleQ     <- arbitraryBoolPair
    check       <- arbitraryBoolPair
    enPass      <- arbitraryMaybePosition
    selectedSq  <- arbitraryMaybePosition
    return GameState {
      board = brd,
      currentPlayer = currentP,
      canCastleKingSide = castleK,
      canCastleQueenSide = castleQ,
      isCheck = check,
      enPassant = enPass,
      selectedSquare = selectedSq
    }

arbitraryBoolPair :: Gen (Bool, Bool)
arbitraryBoolPair = (,) <$> arbitrary <*> arbitrary

arbitraryMaybePosition :: Gen (Maybe Position)
arbitraryMaybePosition = frequency [(1, return Nothing), (9, Just <$> arbitrary)]

arbitraryPosition :: Gen Position
arbitraryPosition = (,) <$> choose (0, 7) <*> choose (0, 7)

-- Test 1. 

prop_swapPlayerTwice :: GameState -> Bool
prop_swapPlayerTwice gameState =
  currentPlayer (swapPlayer (swapPlayer gameState)) == currentPlayer gameState

-- Test 2.

prop_legalPawnMoves :: GameState -> Position -> Bool
prop_legalPawnMoves gs pos = all (isLegalPawnMove gs pos) (legalMovesForPiece gs pos True)

-- Test 3.

prop_legalKnightMoves :: GameState -> Position -> Bool
prop_legalKnightMoves gs pos = all (isLegalKnightMove gs pos) (legalMovesForPiece gs pos True)

