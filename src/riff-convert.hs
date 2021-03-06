module Main where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath (splitExtension)
import Data.Riff

data Flag
   = Help
   | ToRifx
   deriving(Eq, Show)

options :: [OptDescr Flag]
options = 
   [ Option "h" ["help"] (NoArg Help) "prints this help message"
   , Option "x" ["rifx"] (NoArg ToRifx) "converts to the RIFX format. Converts to RIFF otherwise."
   ]

main = do
   args <- getArgs
   let (flags, extras, _) = getOpt Permute options args
   handleFlags flags extras
   
handleFlags :: [Flag] -> [String] -> IO ()
handleFlags flags files = 
   if Help `elem` flags
      then putStrLn $ usageInfo "riff-convert" options
      else sequence_ $ fmap (convert toRifx) files
   where
      toRifx = ToRifx `elem` flags

convert :: Bool -> FilePath -> IO ()
convert toRifx filePath = do
   putStr $ "> Reading " ++ filePath ++ "..."
   withRiffFile filePath $ \result -> case result of
      Left (pos, error) -> putStrLn $ error ++ " (Offset: " ++ show pos ++ ")"
      Right riffFile -> do
         putStr "converting..."
         assembleRiffFile newFilename $ convertRep riffFile
         putStrLn "[DONE]"
   where
      newFilename = filename ++ ".converted" ++ ext
      (filename, ext) = splitExtension filePath

      convertRep riffFile = riffFile { riffFileType = riffFT } 
      riffFT = if toRifx then RIFX else RIFF
