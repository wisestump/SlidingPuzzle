module SimpleGameState where

import qualified Data.Map as Map 
import System.Random

data Cell = Empty | Piece Int deriving (Show)
data Move = Left | Right | Up | Down deriving (Enum, Bounded, Show, Read)
data GameData = GameData { state :: GameState, emptyInd :: IndType, size :: Int }

type IndType = (Int,Int)
type GameState = Map.Map IndType Cell

makeMove :: GameData -> Move -> GameData
makeMove = undefined

startState :: RandomGen g => g -> GameData
startState = undefined