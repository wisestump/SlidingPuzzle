{-# LANGUAGE RecordWildCards #-}
module SimpleGameState where

import qualified Data.Map as Map 
import System.Random
import Data.Maybe

data Cell = Empty | Piece Int deriving (Show)
data Move = Left | Right | Up | Down deriving (Enum, Bounded, Show, Read)
data GameData = GameData { state :: GameState, emptyInd :: IndType, size :: Int }

type IndType = (Int,Int)
type GameState = Map.Map IndType Cell

makeMove :: GameData -> Move -> GameData
makeMove g@GameData{..} move = do
  case (indMove size emptyInd move) of
    Just next -> do
      let nextState = swapElems emptyInd next state
      let
        state = nextState
        emptyInd = next in GameData{..}
    Nothing -> g

indMove :: Int -> IndType -> Move -> Maybe IndType
indMove size empty move = let (x,y) = indMove' empty move in
  if x < 0 || x > size-1
  || y < 0 || y > size-1 then
    Nothing
  else
    Just (x,y)
 where
  indMove' :: IndType -> Move -> IndType
  indMove' (x,y) SimpleGameState.Left = (x-1,y)
  indMove' (x,y) SimpleGameState.Right = (x+1,y)
  indMove' (x,y) Up = (x,y-1)
  indMove' (x,y) Down = (x,y+1)

swapElems :: IndType -> IndType -> GameState -> GameState
swapElems i j state = do
  let x = Map.lookup i state
  let y = Map.lookup j state
  
  if (isNothing x || isNothing y) then do
    let s1 = Map.update (\z -> y) i state
    Map.update (\z -> x) j s1
  else
    state

startState :: RandomGen g => g -> GameData
startState = undefined