module Main where

import System.Environment

import GUI
import ConsoleUI

data ParsedArgs = 
  RunGUI Int     |
  RunConsole Int |
  Help           |
  Unknown

parseArgs [mode, size] = 
  case mode of
    "gui" -> RunGUI (read size) 
    "console" -> RunConsole (read size) 
parseArgs ["help"] = Help
parseArgs _ = Unknown

execArgs (RunGUI size) = startGUI size
execArgs (RunConsole size) = startConsoleUI size
execArgs Help = do
  putStrLn "Usage: main <mode> <field-size>"
  putStrLn "mode: gui/console"
  putStrLn "field-size: integer"
execArgs Unknown = putStrLn "Unknown command (use :main help)"

main :: IO ()
main = do
  args <- getArgs
  execArgs $ parseArgs args