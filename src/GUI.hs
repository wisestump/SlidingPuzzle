{-# LANGUAGE LambdaCase, RecordWildCards #-}

module GUI where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

import System.Random
import qualified Data.Map as Map

import qualified SimpleGameState as GS

fps :: Int
fps = 30

pieceSize :: Float
pieceSize = 100

window :: Int -> Display
window fieldSize = InWindow "Sliding puzzle" ((round pieceSize) * fieldSize, (round pieceSize) * fieldSize) (0, 0)

backgroundColor, pieceColor, textColor :: Color
backgroundColor = white
pieceColor = orange
textColor = black
gridColor = black

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

viewPort :: Int -> ViewPort
viewPort fieldSize = ViewPort (both (negate . (/ 2) . (subtract pieceSize)) $ pieceToScreen (fieldSize, fieldSize)) 0 1

updater :: t -> a -> a
updater _ = id

handler :: Event -> GS.GameData -> GS.GameData
handler (EventKey (SpecialKey KeyUp) Down _ _) gs = GS.makeMoveWithCheck GS.Down gs
handler (EventKey (SpecialKey KeyDown) Down _ _) gs = GS.makeMoveWithCheck GS.Up gs
handler (EventKey (SpecialKey KeyLeft) Down _ _) gs = GS.makeMoveWithCheck GS.Right gs
handler (EventKey (SpecialKey KeyRight) Down _ _) gs = GS.makeMoveWithCheck GS.Left gs

handler _ gs = gs

label :: String -> Picture
label str = let offset = fromIntegral $ -5 * (length str) in translate offset offset $ scale 0.15 0.15 $ color textColor $ text str

renderer :: GS.GameData -> Picture
renderer GS.GameData{..} = applyViewPortToPicture (viewPort size) $ pictures $ pieces ++ grid
  where
    pieces = map 
      (\((i, j), cellState) -> place (i, size - j - 1) $ \case { GS.Empty -> empty; GS.Piece num -> piece num } $ cellState) 
      $ Map.assocs state
    grid = map (\pos -> place pos $ gridCell) [(i, j) | i <- [0 .. size - 1], j <- [0 .. size - 1]]
    gridCell = color gridColor $ rectangleWire pieceSize pieceSize
    place pos = uncurry translate (pieceToScreen pos)
    piece num = pictures [color pieceColor $ rectangleSolid pieceSize pieceSize, label $ show num]
    empty = color backgroundColor $ rectangleSolid pieceSize pieceSize

pieceToScreen :: (Int, Int) -> (Float, Float)
pieceToScreen = both ((* pieceSize) . fromIntegral)

startGUI :: Int -> IO ()
startGUI size = do
  gen <- newStdGen
  play (window size) backgroundColor fps (GS.startState size gen) renderer handler updater