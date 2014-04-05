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
         children <- parseChunkList (size - 4)
         when (size `mod` 2 == 1) $ skip 1
         return RiffChunkParent
            { riffChunkId = id
            , riffChunkSize = size
            , riffFormTypeInfo = formType
            , riffChildren = children
            }
      else do
         -- TODO do we need to consider byte boundaries here?
         riffData <- getNWords (fromIntegral size)
         when (size `mod` 2 == 1) $ skip 1
         return RiffChunkChild
            { riffChunkId = id
            , riffChunkSize = size
            , riffData = riffData
            }

padToWord :: Word32 -> Word32
padToWord x = if x `mod` 2 == 0
   then x
   else x + 1

paddedChunkSize = padToWord . riffChunkSize

-- TODO worry about pad bytes
parseChunkList :: RiffChunkSize -> Get [RiffChunk]
parseChunkList totalSize
   | totalSize <= 0 = return []
   | otherwise = do
      nextChunk <- getRiffChunk
      following <- parseChunkList (totalSize - (8 + paddedChunkSize nextChunk))
      return $ nextChunk : following

parentChunkNames :: [String]
parentChunkNames = ["RIFF", "LIST"]
