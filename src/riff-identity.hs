module Main where

import Data.Riff

import System.Environment (getArgs)

main = do
   args <- getArgs
   case args of
      [] -> putStrLn "You need to give this a riff file!"
      xs -> do
         putStrLn "Parsing and reassembling RIFF files:"
         sequence_ $ fmap reassembleFile xs
         putStrLn "Finished reassembling RIFF files."

reassembleFile :: FilePath -> IO ()
reassembleFile filePath = do
   putStr $ "> " ++ filePath ++ " ..."
   withRiffFile filePath $ \e -> case e of
      Left (_, error) -> putStrLn "[FAILED]" >> putStrLn error
      Right riffFile -> assembleRiffFile (filePath ++ ".clone") riffFile
   putStrLn "[Done]"
