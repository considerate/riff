module Main where

import Data.Riff

import System.Environment (getArgs)

main = do
   args <- getArgs
   case args of
      [] -> putStrLn "You need to give this a riff file!"
      (x:_) ->
         withRiffFile x $ \e -> case e of
            Left (_, error) -> putStrLn error
            Right riffFile -> assembleRiffFile (x ++ ".clone") riffFile
