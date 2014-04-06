module Data.Riff.Operations where

import Data.Riff.RiffData

calculateFileLength :: RiffFile -> RiffChunkSize
calculateFileLength (RiffFile _ _ children) = idLength + childHeaderLength + childrenLength
   where 
      idLength = 4
      childHeaderLength = fromIntegral (8 * length children)
      childrenLength = sum $ fmap calculateChunkLength children

calculateChunkLength :: RiffChunk -> RiffChunkSize
calculateChunkLength (RiffChunkChild _ chunkData) = fromIntegral $ length chunkData
calculateChunkLength (RiffChunkParent _ children) = idLength + childHeaderLength + childrenLength
   where 
      idLength = 4
      childHeaderLength = fromIntegral (8 * length children)
      childrenLength = sum $ fmap calculateChunkLength children
