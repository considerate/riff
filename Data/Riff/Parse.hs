{-|
Description : The module that allows parsing of a RIFF / RIFX file.
Copyright   : (c) Robert Massaioli, 2014
License     : MIT
Maintainer  : robertmassaioli@gmail.com
Stability   : experimental

This module allows the parsing of RIFF files in pure Haskell. You can parse a RIFF file using the 
methods provided in this module. For example, if you wanted to parse a RIFF file and print it out
then you could:

> main = withRiffFile "path/to/file.riff" print

And that will print the RIFF file, in gory details, to the screen.
-}
module Data.Riff.Parse
   ( withRiffFile
   , parseRiffFileStream
   , getRiffFile
   ) where

import Data.Riff.RiffData
import Data.Riff.InternalUtil

import Control.Monad (when, replicateM)
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)
import Data.Word (Word8)
import GHC.IO.IOMode (IOMode(..))
import System.IO (withBinaryFile)

-- | Given a FilePath you can provide a function that will be given either a ParseError or
-- an actual RiffFile to process. It is important to note that you should do all of the
-- processing of the RiffFile in the provided action because the file will be closed at
-- the end of this function.
withRiffFile :: FilePath                              -- ^ The file that will be read.
             -> (Either ParseError RiffFile -> IO ()) -- ^ An action to perform on the potentialy 
                                                      -- parsed file
             -> IO ()                                 -- ^ The resultant IO action.
withRiffFile filePath action = withBinaryFile filePath ReadMode $ \h -> do
   riffData <- fmap parseRiffFileStream (BL.hGetContents h)
   action riffData

-- | You can parse a raw ByteString and try and convert it into a RiffFile. This will give
-- a best attempt at parsing the data and, if success is not possible, will give you a
-- ParseError.
parseRiffFileStream :: BL.ByteString               -- A lazy bytestring for input.
              -> Either ParseError RiffFile        -- The result of our attempted parse.
parseRiffFileStream input =
   case runGetOrFail getRiffFile input of
      Left (_, offset, error) -> Left (offset, error)
      Right (_, _, result) -> Right result

data ParseContext = ParseContext
   { getSize :: Get RiffChunkSize
   }

-- | A binary instance of RiffFile so that you can parse one wherever you find it.
getRiffFile :: Get RiffFile
getRiffFile = do
   id <- getIdentifier
   (context, fileType) <- case id of
      "RIFF" -> return (leContext, RIFF)
      "RIFX" -> return (beContext, RIFX)
      _ -> do
         fail $ "RIFF file not allowed to start with chunk id: '" ++ id ++ "'. Must start with either RIFF or RIFX"
   size <- getSize $ context
   riffType <- getIdentifier
   contents <- parseChunkList context (size - 4)
   return RiffFile
      { riffFileType = fileType
      , riffFileFormatType = riffType
      , riffFileChildren = contents
      }
   where
      leContext = ParseContext getWord32le
      beContext = ParseContext getWord32be

parseChunkList :: ParseContext -> RiffChunkSize -> Get [RiffChunk]
parseChunkList _        0         = return []
parseChunkList context  totalSize = do
   (nextChunk, dataSize) <- getRiffChunk context
   -- No matter what type of chunk it is tehre will be 8 bytes taken up by the id and size
   let chunkSize = 8 + padToWord dataSize
   if totalSize <= chunkSize
      then return [nextChunk]
      else do
         following <- parseChunkList context (totalSize - chunkSize)
         return $ nextChunk : following

getRiffChunk :: ParseContext -> Get (RiffChunk, RiffChunkSize)
getRiffChunk context = do
   id <- getIdentifier
   size <- getSize $ context
   if id == "LIST"
      then do
         guardListSize id size
         formType <- getIdentifier
         -- Minus 4 because of the formType before that is part of the size
         children <- parseChunkList context (size - 4)
         skipToWordBoundary size
         return (RiffChunkParent
            { riffFormTypeInfo = formType
            , riffChunkChildren = children
            }, size)
      else do
         -- TODO do we need to consider byte boundaries here?
         riffData <- getLazyByteString (fromIntegral size)
         skipToWordBoundary size
         return (RiffChunkChild
            { riffChunkId = id
            , riffData = riffData
            }, size)
   where
      guardListSize id size = when (size < 4) $ do
         fail $ message id size
         where
            message id size = 
               "List Chunk Id '" ++ id 
               ++ "' had chunk size " ++ show size 
               ++ " which is less than 4 and invalid."

skipToWordBoundary :: RiffChunkSize -> Get ()
skipToWordBoundary size = do
   empty <- isEmpty
   when (not empty && size `mod` 2 == 1) $ skip 1

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords 

-- Parsing bytes
byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

getIdentifier = getNChars 4
