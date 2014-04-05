module Data.Riff where

import Data.Riff.ParseExtras

import System.IO (withBinaryFile, hSetBinaryMode)
import GHC.IO.IOMode
import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class

type RiffData = Word8
type RiffChunkSize = Word32
type RiffId = String

-- Important: The first RiffChunk must be a RiffChunkParent with id RIFF.
type RiffFile = RiffChunk
type ParseError = (ByteOffset, String)

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

withRiffFile :: String -> (Either ParseError RiffFile -> IO ()) -> IO ()
withRiffFile filePath action = withBinaryFile filePath ReadMode $ \h -> do
   riffData <- fmap parseRiffData (BL.hGetContents h)
   action riffData

parseRiffData :: BL.ByteString -> Either ParseError RiffFile
parseRiffData input =
   case runGetOrFail (runEitherT getRiffChunk) input of
      Left (_, offset, error) -> Left (offset, error)
      Right (_, _, result) -> result

getRiffChunk :: EitherT ParseError Get RiffChunk
getRiffChunk = do
   id <- lift getIdentifier
   -- TODO will it always be little endian?
   size <- lift getWord32le
   if id `elem` parentChunkNames
      then do
         guardListSize id size
         formType <- lift getIdentifier
         -- Minus 4 because of the formType before that is part of the size
         children <- parseChunkList (size - 4)
         lift $ skipToWordBoundary size
         return RiffChunkParent
            { riffChunkId = id
            , riffChunkSize = size
            , riffFormTypeInfo = formType
            , riffChildren = children
            }
      else do
         -- TODO do we need to consider byte boundaries here?
         riffData <- lift $ getNWords (fromIntegral size)
         lift $ skipToWordBoundary size
         return RiffChunkChild
            { riffChunkId = id
            , riffChunkSize = size
            , riffData = riffData
            }
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

padToWord :: Word32 -> Word32
padToWord x = if x `mod` 2 == 0
   then x
   else x + 1

paddedChunkSize = padToWord . riffChunkSize

parseChunkList :: RiffChunkSize -> EitherT ParseError Get [RiffChunk]
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
