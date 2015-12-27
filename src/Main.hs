module Main where

import System.Environment

import GUI

main :: IO ()
main = do
  [mode, size] <- getArgs
  case mode of
    "gui" -> startGUI (read size)