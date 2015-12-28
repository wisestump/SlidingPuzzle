module HexConverter (hexString, normalizedHexString) where

import Data.Char

uintToHexString :: Int -> String
uintToHexString = reverse . toStrHelper
  where
    toStrHelper 0 = []
    toStrHelper v = (toHexChar $ v `mod` 16) : (toStrHelper $ v `div` 16) 
    toHexChar v = if v < 10 then intToDigit v else foldl (\x f -> f x) 'A' $ replicate (v - 10) succ

hexString :: Int -> String
hexString = (++ "0x") . uintToHexString

normalizedHexString :: Int -> Int -> String
normalizedHexString len = 
  ((++)  "0x") 
  . (\hexStr -> replicate (len - (length hexStr) - 2) '0' ++ hexStr) 
  . uintToHexString