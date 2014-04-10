{-|
Description : The module that allows the assembly of a RIFF / RIFX file.
Copyright   : (c) Robert Massaioli, 2014
License     : MIT
Maintainer  : robertmassaioli@gmail.com
Stability   : experimental

This module allows the assembly of RIFF files in pure Haskell. You can create a RIFF file in 
pure Haskell starting with a RiffFile and building it up until you are ready to write it out 
to a ByteString or Disk. For example, this is how you might construct a RIFF file to be written
out:

> import Data.Riff
> import qualified Data.ByteString.Lazy as BL
> 
> riffFile = RiffFile RIFX "EXPL" children
> 
> children = 
>    [ RiffChunkChild "fst " $ BL.pack [1..11]
>    , RiffChunkChild "snd " $ BL.pack [11..100]
>    ]
> 
> main = assembleRiffFile "example.riff" riffFile

As you can see it is a very simple API that lets you write out data into Riff Files. Have a play 
around with with the examples until you can see how it works and fits together.
-}
module Data.Riff.Assemble 
   ( assembleRiffFile
   , assembleRiffFileStream
   , putRiffFile
   ) where

import Data.Riff.RiffData
import Data.Riff.Operations
import qualified Data.ByteString.Lazy as BL
import System.IO (withBinaryFile, IOMode(..))
import Control.Monad (when)

import Data.Binary.Put
import Data.Char (ord)

-- | Given a file path and a RiffFile representation this will allow you to safely write
-- out a RiffFile to disk. This allows you to save anything that you like in a RiffFile.
-- Just remember that the maximum file size of a RiffFile is bounded by the maximum size
-- of a 32bit integer. The behaviour of this function, should you give it too much data,
-- is undefined.
assembleRiffFile 
   :: FilePath -- ^ The location on the filesystem to save the RiffFile.
   -> RiffFile -- ^ The in-memory representation of a RiffFile to be saved.
   -> IO ()    -- ^ Writing to disk is an IO operatin and we return no results.
assembleRiffFile filePath riffFile = withBinaryFile filePath WriteMode $ \h -> 
   BL.hPut h (assembleRiffFileStream riffFile)

-- | Assembles a RiffFile into it's representation in a Lazy ByteString. 
assembleRiffFileStream 
   :: RiffFile       -- ^ The RIFF file to be written out. 
   -> BL.ByteString  -- ^ The resultant stream of bytes representing the file.
assembleRiffFileStream = runPut . putRiffFile

-- | A Binary put instance so that you can write a riff file right out to any stream.
putRiffFile 
   :: RiffFile    -- ^ The riff file to write out to the stream.
   -> Put         -- ^ The Put monad that will do the writing.
putRiffFile riffFile = do
   printHeader . riffFileType $ riffFile -- Do not need safeId, chosen to be correrct
   putSize context . calculateFileLength $ riffFile
   putString . safeId . riffFileFormatType $ riffFile
   sequence_ $ fmap (putRiffChunk context) (riffFileChildren riffFile)
   where
      context = getContext . riffFileType $ riffFile
   -- TODO do we need to word align the end of a riff file?

getContext :: RiffFileType -> AssemblyContext
getContext RIFF = AssemblyContext putWord32le
getContext RIFX = AssemblyContext putWord32be

data AssemblyContext = AssemblyContext
   { putSize :: RiffChunkSize -> Put
   }

putRiffChunk :: AssemblyContext -> RiffChunk -> Put
putRiffChunk context chunk@(RiffChunkChild _ _) = do
   putString . safeId . riffChunkId $ chunk
   let chunkSize = calculateChunkLength chunk
   putSize context chunkSize
   putLazyByteString . riffData $ chunk
   maybeFillBlank chunkSize
putRiffChunk context chunk@(RiffChunkParent _ _) = do
   putString "LIST" -- Do not need to pass through safeId, chosen to be correct
   let chunkSize = calculateChunkLength chunk
   putSize context chunkSize
   putString . safeId . riffFormTypeInfo $ chunk
   sequence_ $ fmap (putRiffChunk context) (riffChunkChildren chunk)
   maybeFillBlank chunkSize

maybeFillBlank :: RiffChunkSize -> Put
maybeFillBlank chunkSize = when (chunkSize `mod` 2 == 1) putBlankByte

putBlankByte = putWord8 0

printHeader :: RiffFileType -> Put
printHeader RIFF = putString "RIFF"
printHeader RIFX = putString "RIFX"

putString :: String -> Put
putString = sequence_ . fmap (putWord8 . fromIntegral . ord)

safeId :: RiffId -> RiffId
safeId input = take 4 $ input ++ repeat ' '
