module Data.Riff where

import Data.Riff.ParseExtras

import System.IO (withBinaryFile, hSetBinaryMode)
import GHC.IO.IOMode
import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)

type RiffData = Word8
type RiffChunkSize = Word32
type RiffId = String

-- Important: The first RiffChunk must be a RiffChunkParent with id RIFF.
type RiffFile = RiffChunk

data RiffChunk 
   = RiffChunkChild
      { riffChunkId :: RiffId
      , riffChunkSize :: RiffChunkSize
      , riffData :: [RiffData]
      }
   | RiffChunkParent
      { riffChunkId :: RiffId
      , riffChunkSize :: RiffChunkSize
      , riffFormTypeInfo :: RiffId
      , riffChildren :: [RiffChunk]
      }
   deriving (Eq, Show)

withRiffFile :: String -> (RiffFile -> IO ()) -> IO ()
withRiffFile filePath action = withBinaryFile filePath ReadMode $ \h -> do
   riffData <- fmap parseRiffData (BL.hGetContents h)
   action riffData

parseRiffData :: BL.ByteString -> RiffFile
parseRiffData = runGet getRiffChunk

getRiffChunk :: Get RiffChunk
getRiffChunk = do
   id <- getIdentifier
   -- TODO will it always be little endian?
   size <- getWord32le
   if id `elem` parentChunkNames
      then do
         formType <- getIdentifier
         -- Minus 4 because of the formType before that is part of the size
         children <- parseChunkList (size - 4)
         skipToWordBoundary size
         return RiffChunkParent
            { riffChunkId = id
            , riffChunkSize = size
            , riffFormTypeInfo = formType
            , riffChildren = children
            }
      else do
         -- TODO do we need to consider byte boundaries here?
         riffData <- getNWords (fromIntegral size)
         skipToWordBoundary size
         return RiffChunkChild
            { riffChunkId = id
            , riffChunkSize = size
            , riffData = riffData
            }

skipToWordBoundary :: RiffChunkSize -> Get ()
skipToWordBoundary size = do
   empty <- isEmpty
   when (not empty && size `mod` 2 == 1) $ skip 1

padToWord :: Word32 -> Word32
padToWord x = if x `mod` 2 == 0
   then x
   else x + 1

paddedChunkSize = padToWord . riffChunkSize

parseChunkList :: RiffChunkSize -> Get [RiffChunk]
parseChunkList 0         = return []
parseChunkList totalSize = do
   nextChunk <- getRiffChunk
   -- No matter what type of chunk it is tehre will be 8 bytes taken up by the id and size
   let chunkSize = 8 + paddedChunkSize nextChunk
   if totalSize <= chunkSize
      then return [nextChunk]
      else do
         following <- parseChunkList (totalSize - chunkSize)
         return $ nextChunk : following

parentChunkNames :: [String]
parentChunkNames = ["RIFF", "LIST"]
