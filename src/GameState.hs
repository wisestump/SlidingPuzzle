{-# LANGUAGE FlexibleContexts, ConstraintKinds, RecordWildCards #-}

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
import Control.Monad.Trans.Identity
import Control.Monad.Identity
import Data.Maybe

data Cell = Empty | Piece Int deriving (Show)
data Move = Left | Right | Up | Down deriving (Enum, Bounded, Show)

type IndType = (Int,Int)
type GameState s = STArray s IndType Cell
-- ConstraintKinds
type GameStateMonad s m =  MArray (STArray s) Cell m
data GameData s = GameData { state :: GameState s, emptyInd :: IndType }
data FieldData = FieldData { size :: Int }

allMoves :: [Move]
allMoves = [minBound :: Move ..]

finishData :: ST s (GameData s)
finishData = undefined

-- RecordWildCards
-- FlexibleContexts 
makeMove :: (MS.MonadState (GameData s) m, GameStateMonad s m, MonadReader FieldData m) => Move -> m ()
makeMove move = do
  GameData{..} <- MS.get
  FieldData{..} <- ask
  case (indMove size emptyInd move) of
    Just next -> do
      swapElems emptyInd next state
      let emptyInd = next in MS.put GameData{..}
    Nothing -> return ()

indMove :: Int -> IndType -> Move -> Maybe IndType
indMove size empty move = let (x,y) = indMove' empty move in
  if x < 0 || x > size-1
  || y < 0 || y > size-1 then
    Nothing
  else
    Just (x,y)
 where
  indMove' :: IndType -> Move -> IndType
  indMove' (x,y) Left = (x-1,y)
  indMove' (x,y) Right = (x+1,y)
  indMove' (x,y) Up = (x,y-1)
  indMove' (x,y) Down = (x,y+1)

moveFromIndex :: (MS.MonadState (GameData s) m, GameStateMonad s m, MonadReader FieldData m) => IndType -> m (Maybe Move)
moveFromIndex (x,y) = do
  GameData{..} <- MS.get
  FieldData{..} <- ask
  let (xe,ye) = emptyInd
  return $ vectorToMove (x - xe, y - ye)
 where
  vectorToMove (-1,0) = Just Left
  vectorToMove (1,0) = Just Right
  vectorToMove (0,-1) = Just Up
  vectorToMove (0,1) = Just Down
  vectorToMove _ = Nothing

isMoveCorrect :: (MS.MonadState (GameData s) m, GameStateMonad s m, MonadReader FieldData m) => Move -> m Bool
isMoveCorrect move = do
  GameData{..} <- MS.get
  FieldData{..} <- ask
  return $ isJust $ indMove size emptyInd move

nextMoves :: (MS.MonadState (GameData s) m, GameStateMonad s m, MonadReader FieldData m) => m [Move]
nextMoves = do
  GameData{..} <- MS.get
  FieldData{..} <- ask
  return $ filter (isJust . indMove size emptyInd) allMoves

swapElems :: (MArray (STArray s) e m, Ix i) => i -> i -> STArray s i e -> m ()
swapElems i j arr = do
  vi <- readArray arr i
  vj <- readArray arr j
  writeArray arr i vj
  writeArray arr j vi