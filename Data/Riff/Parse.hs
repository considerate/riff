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
   , parseRiffData
   ) where

import Data.Riff.RiffData

import Control.Monad (when, replicateM)
import Control.Monad.Trans.Either (EitherT(..), left, right)
import Control.Monad.Trans.Class
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
   riffData <- fmap parseRiffData (BL.hGetContents h)
   action riffData

-- | You can parse a raw ByteString and try and convert it into a RiffFile. This will give
-- a best attempt at parsing the data and, if success is not possible, will give you a
-- ParseError.
parseRiffData :: BL.ByteString               -- A lazy bytestring for input.
              -> Either ParseError RiffFile  -- The result of our attempted parse.
parseRiffData input =
   case runGetOrFail (runEitherT getRiffStart) input of
      Left (_, offset, error) -> Left (offset, error)
      Right (_, _, result) -> result

data ParseContext = ParseContext
   { getSize :: Get RiffChunkSize
   }

getRiffStart :: EitherT ParseError Get RiffFile
getRiffStart = do
   id <- lift getIdentifier
   (context, fileType) <- case id of
      "RIFF" -> right (leContext, RIFF)
      "RIFX" -> right (beContext, RIFX)
      _ -> do
         read <- lift bytesRead 
         left (read, "RIFF file not allowed to start with chunk id: '" ++ id ++ "'. Must start with either RIFF or RIFX")
   size <- lift . getSize $ context
   riffType <- lift getIdentifier
   contents <- parseChunkList context (size - 4)
   return RiffFile
      { riffFileType = fileType
      , riffFileFormatType = riffType
      , riffFileChildren = contents
      }
   where
      leContext = ParseContext getWord32le
      beContext = ParseContext getWord32be

parseChunkList :: ParseContext -> RiffChunkSize -> EitherT ParseError Get [RiffChunk]
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

getRiffChunk :: ParseContext -> EitherT ParseError Get (RiffChunk, RiffChunkSize)
getRiffChunk context = do
   id <- lift getIdentifier
   size <- lift . getSize $ context
   if id == "LIST"
      then do
         guardListSize id size
         formType <- lift getIdentifier
         -- Minus 4 because of the formType before that is part of the size
         children <- parseChunkList context (size - 4)
         lift $ skipToWordBoundary size
         return (RiffChunkParent
            { riffFormTypeInfo = formType
            , riffChunkChildren = children
            }, size)
      else do
         -- TODO do we need to consider byte boundaries here?
         riffData <- lift $ getNWords (fromIntegral size)
         lift $ skipToWordBoundary size
         return (RiffChunkChild
            { riffChunkId = id
            , riffData = riffData
            }, size)
   where
      guardListSize id size = when (size < 4) $ do
         read <- lift bytesRead
         left (read, message id size)
         where
            message id size = 
               "List Chunk Id '" ++ id 
               ++ "' had chunk size " ++ show size 
               ++ " which is less than 4 and invalid."

skipToWordBoundary :: RiffChunkSize -> Get ()
skipToWordBoundary size = do
   empty <- isEmpty
   when (not empty && size `mod` 2 == 1) $ skip 1

padToWord :: RiffChunkSize -> RiffChunkSize
padToWord x = if x `mod` 2 == 0
   then x
   else x + 1

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords 

-- Parsing bytes
byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

getIdentifier = getNChars 4
