module Data.Riff.Assemble 
   ( assembleRiffFile
   , assembleRiffFileStream
   ) where

import Data.Riff.RiffData
import Data.Riff.Operations
import qualified Data.ByteString.Lazy as BL
import System.IO (withBinaryFile, IOMode(..))
import Control.Monad (when)

import Data.Binary.Put
import Data.Char (ord)

assembleRiffFile :: FilePath -> RiffFile -> IO ()
assembleRiffFile filePath riffFile = withBinaryFile filePath WriteMode $ \h -> do
   BL.hPut h (assembleRiffFileStream riffFile)

assembleRiffFileStream :: RiffFile -> BL.ByteString
assembleRiffFileStream = runPut . writeRiffFile

writeRiffFile :: RiffFile -> Put
writeRiffFile riffFile = do
   printHeader . riffFileType $ riffFile
   putWord32le . calculateFileLength $ riffFile
   putString . riffFileFormatType $ riffFile
   sequence_ $ fmap writeRiffChunk (riffFileChildren riffFile)
   -- TODO do we need to word align the end of a riff file?

writeRiffChunk :: RiffChunk -> Put
writeRiffChunk chunk@(RiffChunkChild _ _) = do
   putString . riffChunkId $ chunk
   let chunkSize = calculateChunkLength chunk
   putWord32le chunkSize
   sequence_ $ fmap putWord8 (riffData chunk)
   when (chunkSize `mod` 2 == 1) putBlankByte
writeRiffChunk chunk@(RiffChunkParent _ _) = do
   putString "LIST"
   let chunkSize = calculateChunkLength chunk
   putWord32le chunkSize
   putString . riffFormTypeInfo $ chunk
   sequence_ $ fmap writeRiffChunk (riffChunkChildren chunk)
   when (chunkSize `mod` 2 == 1) putBlankByte

putBlankByte = putWord8 0

printHeader :: RiffFileType -> Put
printHeader RIFF = putString "RIFF"
printHeader RIFX = putString "RIFX"

putString :: String -> Put
putString = sequence_ . fmap (putWord8 . fromIntegral . ord)
