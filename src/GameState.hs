{-# LANGUAGE FlexibleContexts, ConstraintKinds, AllowAmbiguousTypes, RecordWildCards #-}

module GameState where

import Data.STRef
import Control.Monad.ST
import qualified Control.Monad.State.Class as MS
import Control.Monad.Reader
import Control.Monad
import Data.Array
import Data.Array.ST
import Data.Array.MArray
import Prelude hiding (Either(Left),Either(Right))

data Cell = Empty | Piece Int deriving (Show)
data Move = Left | Right | Up | Down deriving (Enum, Bounded, Show)

type IndType = (Int,Int)
type GameState s = STArray s IndType Cell
type GameStateMonad s m =  MArray (STArray s) Cell m
data GameData s = GameData { state :: GameState s, emptyInd :: IndType }
data FieldData = FieldData { size :: Int }

allMoves :: [Move]
allMoves = [minBound :: Move ..]

finishData :: ST s (GameData s)
finishData = undefined

makeMove :: (MS.MonadState (GameData s) m, GameStateMonad s m, MonadReader FieldData m) => Move -> m ()
makeMove move = do
  GameData{..} <- MS.get
  FieldData{..} <- ask
  let next = indMove size emptyInd move
  swapElems emptyInd next state
 where

  indMove :: Int -> IndType -> Move -> IndType
  indMove size empty move = let (x,y) = indMove' empty move in
    if x < 0 || x > size-1
    || y < 0 || y > size-1 then
      error "Incorrect move"
    else
      (x,y)

  indMove' :: IndType -> Move -> IndType
  indMove' (x,y) Left = (x-1,y)
  indMove' (x,y) Right = (x+1,y)
  indMove' (x,y) Up = (x,y+1)
  indMove' (x,y) Down = (x,y-1)

swapElems :: (MArray (STArray s) e m, Ix i) => i -> i -> STArray s i e -> m ()
swapElems i j arr = do
  vi <- readArray arr i
  vj <- readArray arr j
  writeArray arr i vj
  writeArray arr j vi
