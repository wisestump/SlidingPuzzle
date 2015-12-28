{-# LANGUAGE LambdaCase, RecordWildCards #-}

module ConsoleUI where

import Control.Monad.State hiding (state)
import Control.Monad.Reader
import Data.Array.IO
import Data.List.Split
import System.Random
import Text.Read (readMaybe)

import GameState
import HexConverter

gameLoop :: ReaderT FieldData (StateT GameData IO) ()
gameLoop = do
  printField
  GameData{..} <- get
  if emptyInd == (-1, -1) then win else loop
  where
    loop = do
      liftIO $ putStr $ "Your turn? "
      move <- liftIO $ getLine
      case move of
        "help" -> help
        "exit" -> exit
        otherwise -> \case { Just m -> do { makeMoveWithCheck m; gameLoop }; Nothing -> errorInput } 
                     $ (readMaybe move :: Maybe Move)

    win = liftIO $ putStrLn $ "You win!"
    exit = liftIO $ putStrLn $ "closing..."
    help = do
      liftIO $ putStrLn $ "Moving: Up, Down, Left, Right; Quit: exit"
      loop
    errorInput = do
      liftIO $ putStrLn "Unknown command (use help)"
      loop

hDelim, vDelim, crossDelim :: Char
hDelim = '-'
vDelim = '|'
crossDelim = '+'

horizontalDelim :: Int -> Int -> String
horizontalDelim columns width = 
  (++ [crossDelim]) 
  $ concat 
  $ replicate columns 
  $ crossDelim : replicate width hDelim

rowPart :: Int -> [String] -> String
rowPart indent = 
  (++ [vDelim])
  . concat
  . map (\v -> let indtStr = replicate indent ' ' in vDelim : indtStr ++ v ++ indtStr)

row :: Int -> Int -> [String] -> String
row cellWidth cellHeight elList = 
  let
    elLen = length $ head elList
    indent = ceiling . (/ 2) . fromIntegral . subtract elLen $ cellWidth
    blank = rowPart 0 $ replicate (length elList) $ replicate cellWidth ' ' 
    elemStr = rowPart indent elList
    halfHeight = cellHeight `div` 2
    blankPartList = replicate halfHeight blank 
  in
    unlines $ blankPartList ++ [elemStr] ++ blankPartList  

pieceToStr :: Int -> Cell -> String
pieceToStr len = 
  \case
    Piece i -> normalizedHexString len i
    Empty -> replicate len ' '

field size = (++ [Empty]) $ map (\i -> Piece i) [1 .. size ^ 2 - 1]

printField :: ReaderT FieldData (StateT GameData IO) ()
printField = do
  FieldData{..} <- ask
  GameData{..} <- get
  stArray <- liftIO $ getElems state
  let hDelimStr = horizontalDelim fieldSize cellW
      fieldSize = size
      maxVal = fieldSize ^ 2
      maxLen = length $ hexString maxVal
      cellW = maxLen + 2
      cellH = cellW `div` 2
  liftIO $ putStrLn hDelimStr
  mapM_ (liftIO . putStr . (++ (hDelimStr ++ "\n")) 
          . (row cellW cellH) 
          . map (pieceToStr maxLen)) 
        $ chunksOf fieldSize $ stArray

start = do
  gen <- liftIO newStdGen
  startState gen
  gameLoop

startConsoleUI size = do
  arr <- newListArray ((0, 0), (0, 0)) [Empty]
  runStateT (runReaderT start (FieldData size)) (GameData arr (0, 0))
  return ()