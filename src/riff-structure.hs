module Main where

import Data.Riff
import System.Environment (getArgs)
import Data.List (intersperse)
import Control.Monad (when)
import Data.Char (chr)

main = do
   args <- getArgs
   case args of
      [] -> error "No filename provided to scan. Please give me a file."
      xs -> sequence_ . intersperse putNewline $ fmap processFile xs

processFile :: String -> IO ()
processFile filePath = do
   putStr "File: "
   print filePath
   withRiffFile filePath $ \potential -> case potential of
      Left (offset, error) -> putStrLn $ error ++ " (Offset: " ++ show offset ++ ")"
      Right riffFile -> printRiffFile startContext riffFile

data PrintContext = PrintContext
   { indentation :: Int
   , printValue :: Bool
   }
   deriving (Show)

startContext :: PrintContext
startContext = PrintContext 0 False

printRiffFile :: PrintContext -> RiffFile -> IO ()
printRiffFile context (RiffFile riffType size formatType children) = do
   printWithIndent context (typeName riffType)
   putStrLn $ " (" ++ formatType ++ ") [" ++ show size ++ "]"
   mapM_ (printRiffChunk nextContext) children
   where
      nextContext = PrintContext
         { indentation = 1 + indentation context
         , printValue = False
         }

      typeName RIFF = "RIFF"
      typeName RIFX = "RIFX"

printRiffChunk :: PrintContext -> RiffChunk -> IO ()
printRiffChunk context (RiffChunkChild id size value) = do
   printWithIndent context id
   putStr $ " [" ++ show size ++ "]"
   when (printValue context) $ do
      putStr ": "
      inQuotes (putStr . fmap (chr . fromIntegral) $ takeWhile (/= 0) value)
   putNewline
printRiffChunk context (RiffChunkParent size typeName children) = do
   printWithIndent context "LIST"
   putStrLn $ " (" ++ typeName ++ ") [" ++ show size ++ "]"
   mapM_ (printRiffChunk nextContext) children
   where
      nextContext = PrintContext
         { indentation = 1 + indentation context
         , printValue = typeName == "INFO"
         }

inQuotes :: IO () -> IO ()
inQuotes action = do
   putChar '"'
   action
   putChar '"'

printWithIndent :: PrintContext -> String -> IO ()
printWithIndent context value = do
   putStr . concat . take (indentation context) $ repeat "   "
   putStr value

putNewline = putStrLn ""
