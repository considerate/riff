module Main where

import Data.Riff
import System.Environment (getArgs)

main = do
   args <- getArgs
   case args of
      [] -> error "No filename provided to scan. Please give me a file."
      x:_ -> withRiffFile x print

