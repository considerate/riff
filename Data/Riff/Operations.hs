module Data.Riff.Operations 
   ( calculateFileLength
   , calculateChunkLength
   ) where

import Data.Riff.RiffData

calculateFileLength :: RiffFile -> RiffChunkSize
calculateFileLength (RiffFile _ _ children) = 
   idLength + childHeaderLength children + childrenLength children

calculateChunkLength :: RiffChunk -> RiffChunkSize
calculateChunkLength (RiffChunkChild _ chunkData) = fromIntegral $ length chunkData
calculateChunkLength (RiffChunkParent _ children) = 
   idLength + childHeaderLength children + childrenLength children

idLength = 4
childHeaderLength children = fromIntegral (8 * length children)
childrenLength = sum . fmap calculateChunkLength
