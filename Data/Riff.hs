module Data.Riff 
   ( RiffChunk(..)
   , RiffFile(..)
   , RiffFileType(..)
   , RiffData
   , RiffChunkSize
   , RiffId
   , ParseError
   , withRiffFile
   , parseRiffData
   ) where

import Data.Riff.ParseExtras

import System.IO (withBinaryFile)
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

type ParseError = (ByteOffset, String)

-- | This is our representation of a RIFF file. These files all have a
-- format type and are composed by one or more nestable data Chunks which we represent
-- with a RiffChunk.
data RiffFile = RiffFile
   { riffFileType :: RiffFileType
   , riffFileSize :: RiffChunkSize
   , riffFileFormatType :: RiffId
   , riffFileChildren :: [RiffChunk]
   }
   deriving (Eq, Show)

-- | There are only two different types of RIFF file: RIFF and RIFX and the difference is
-- in the way that data is encoded inside them.
data RiffFileType 
   = RIFF -- ^ This is the most common type of RIFF file and is in little endian format.
   | RIFX -- ^ This is a rare riff format that uses big endian encoding (otherwise known as
          -- Motorolla byte order)
   deriving(Eq, Show)

-- | A RiffFile is just an alias for a RiffChunk. A RiffFile is merely a nested collection
-- of RiffChunks where the first element must be a list of chunks with the ID RIFF or
-- RIFX. 
data RiffChunk 
   = RiffChunkChild
      { riffChunkId :: RiffId
      , riffChunkSize :: RiffChunkSize
      , riffData :: [RiffData]
      }
   | RiffChunkParent
      { riffChunkSize :: RiffChunkSize
      , riffFormTypeInfo :: RiffId
      , riffChunkChildren :: [RiffChunk]
      }
   deriving (Eq, Show)

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

parseRiffData :: BL.ByteString -> Either ParseError RiffFile
parseRiffData input =
   case runGetOrFail (runEitherT getRiffStart) input of
      Left (_, offset, error) -> Left (offset, error)
      Right (_, _, result) -> result

data ParseContext = ParseContext
   { getSize :: Get Word32
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
      , riffFileSize = size
      , riffFileFormatType = riffType
      , riffFileChildren = contents
      }
   where
      leContext = ParseContext getWord32le
      beContext = ParseContext getWord32be

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
            { riffChunkSize = size
            , riffFormTypeInfo = formType
            , riffChunkChildren = children
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
