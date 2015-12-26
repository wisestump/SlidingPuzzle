module GameState where

import Data.STRef
import Control.Monad.ST
import Control.Monad
import Data.Array
import Data.Array.ST
import Data.Array.MArray

data Cell = Empty | Piece Int deriving (Show)
data Move = Left | Right | Up | Down deriving (Enum, Bounded, Show)

type GameState s = STArray s Int Cell
data GameData s = GameData { state :: GameState s, emptyInd :: Int }

cellCount, fieldWidth :: Int
fieldWidth = 4

cellCount = fieldWidth ^ 2

allMoves :: [Move]
allMoves = [minBound :: Move ..]

finishData :: ST s (GameData s)
finishData = undefined

makeMove :: Move -> GameData s -> ST s (GameData s) 
makeMove = undefined