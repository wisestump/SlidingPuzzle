{-# LANGUAGE RecordWildCards #-}
module SimpleGameState where

import qualified Data.Map as Map 
import System.Random
import Data.Maybe

data Cell = Empty | Piece Int deriving (Show)
data Move = Left | Right | Up | Down deriving (Enum, Bounded, Show, Read)
data GameData = GameData { state :: GameState, emptyInd :: IndType, size :: Int } deriving (Show)

type IndType = (Int,Int)
type GameState = Map.Map IndType Cell

allMoves :: [Move]
allMoves = [minBound :: Move ..]

makeMove :: Move -> GameData -> GameData
makeMove move g@GameData{..} = do
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
  
  if (isNothing x || isNothing y) then
    state
  else do
    let s1 = Map.update (\z -> y) i state
    Map.update (\z -> x) j s1

startState :: RandomGen g => Int -> g -> GameData
startState size g = applyN (size - 1) (makeMove SimpleGameState.Right) $ applyN (size - 1) (makeMove Down) $ randState 
 where
  randState = fst $ foldl (\(state,g) x -> let (move, g1) = randMove g in (makeMove move state,g1)) (finishState size, g) [1..1000]
  randMove g = let (i,g1) = randomR (0, (length allMoves) - 1) g in (allMoves !! i, g1)

applyN :: Int -> (a -> a) -> (a -> a)
applyN = (foldr (.) id.) . replicate

finishState :: Int -> GameData
finishState size = do
  let state = Map.fromList $ zip [(j, i) | i <- [0 .. size - 1], j <- [0 .. size - 1]] $ map (\x -> if x == size*size then Empty else Piece x) [1..size*size]
  let emptyInd = (size-1,size-1)
  GameData{..}