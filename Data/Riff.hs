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
   case runGetOrFail (runEitherT getRiffStart) input of
      Left (_, offset, error) -> Left (offset, error)
      Right (_, _, result) -> result

data ParseContext = ParseContext
   { getSize :: Get Word32
   }

leContext = ParseContext getWord32le
beContext = ParseContext getWord32be

getRiffStart :: EitherT ParseError Get RiffChunk
getRiffStart = do
   id <- lift getIdentifier
   context <- case id of
      "RIFF" -> right leContext
      "RIFX" -> right beContext
      _ -> do
         read <- lift bytesRead 
         left (read, "RIFF file not allowed to start with chunk id: '" ++ id ++ "'. Must start with either RIFF or RIFX")
   size <- lift . getSize $ context
   riffType <- lift getIdentifier
   contents <- parseChunkList context (size - 4)
   return RiffChunkParent
      { riffChunkId = id
      , riffChunkSize = size
      , riffFormTypeInfo = riffType
      , riffChildren = contents
      }

getRiffChunk :: ParseContext -> EitherT ParseError Get RiffChunk
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

parseChunkList :: ParseContext -> RiffChunkSize -> EitherT ParseError Get [RiffChunk]
parseChunkList _        0         = return []
parseChunkList context  totalSize = do
   nextChunk <- getRiffChunk context
   -- No matter what type of chunk it is tehre will be 8 bytes taken up by the id and size
   let chunkSize = 8 + paddedChunkSize nextChunk
   if totalSize <= chunkSize
      then return [nextChunk]
      else do
         following <- parseChunkList context (totalSize - chunkSize)
         return $ nextChunk : following
