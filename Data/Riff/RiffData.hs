-- | This module is responsible for all of the datatypes that are required around the codebase.
module Data.Riff.RiffData where

import Data.Word (Word32)
import Data.Binary.Get (ByteOffset)
import qualified Data.ByteString.Lazy as BL

-- | The data in a riff file is just a stream of bytes.
type RiffData = BL.ByteString

-- | A Riff file is made up exclusively of Riff Chunks and each chunk, as the second piece
-- of data in the chunk, contains it's size. The size never includes the first 8 bytes of
-- the chunk, which are the Chunk Id and the Chunk Size but, in the case of a nested
-- chunk, it does include the four bytes of the Chunk FormType Id.
--
-- According to the specification a chunk size must be represented by four bytes of data.
-- This means that the maximum number of bytes that can be present in a RIFF file is:
--
-- > 2 ^ 32 + 8 = 4294967304 bytes or ~4GB 
-- 
-- If your raw data is larger than that then this file format cannot support it.
type RiffChunkSize = Word32

-- | A RiffId is just a four character string (FourCC). It is usually (but by no means
-- always) chosen to be something that is human readable when converted to ASCII. 
type RiffId = String

-- | Represents an error in the parsing of a Riff File. It contains the location in the
-- file that we read up to and a message of what went wrong.
type ParseError = (ByteOffset, String)

-- | This is our representation of a RIFF file. These files all have a
-- format type and are composed by one or more nestable data Chunks which we represent
-- with a RiffChunk.
data RiffFile = RiffFile 
   { riffFileType :: RiffFileType      -- ^ The type of RIFF file.
   , riffFileFormatType :: RiffId      -- ^ An ID representing the type of data contained within.
   , riffFileChildren :: [RiffChunk]   -- ^ The chunks that make up the file.
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
      , riffData :: RiffData
      }
   | RiffChunkParent
      { riffFormTypeInfo :: RiffId
      , riffChunkChildren :: [RiffChunk]
      }
   deriving (Eq, Show)
