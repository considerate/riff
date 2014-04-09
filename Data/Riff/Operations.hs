{-|
Description : The module provides common operations to perform on RIFF files.
Copyright   : (c) Robert Massaioli, 2014
License     : MIT
Maintainer  : robertmassaioli@gmail.com
Stability   : experimental

This module provides some common operations that you might like to perform on RIFF files. This is 
by no means an exhaustive list but you may find it useful. This is meant to be a collection of 
common methods to make your development time shorter.
-}
module Data.Riff.Operations 
   ( calculateFileLength
   , calculateChunkLength
   , trueFileSize
   ) where

import Data.Riff.RiffData
import Data.Riff.InternalUtil
import qualified Data.ByteString.Lazy as BL

-- | This function calculates the size of the RIFF file if it was written out to disk.
trueFileSize 
   :: RiffFile -- ^ The RIFF file that needs its size calculated.
   -> Integer  -- ^ The size of the RIFF file if it was written to disk.
trueFileSize riffFile = 8 + fromIntegral (calculateFileLength riffFile)

-- | Calculates the size that should be in the ChunkSize location of the RiffFile when it
-- has been written out to disk. If you want to get the true file size of a Riff file once
-- it has been written out to disk then you need to use the trueFileSize function.
calculateFileLength 
   :: RiffFile       -- The RIFF file whose data size we want to calculate.
   -> RiffChunkSize  -- The RiffChunkSize that should be placed in the initial ChunkSize slot.
calculateFileLength (RiffFile _ _ children) = 
   idLength + childHeaderLength children + childrenLength children

-- | Calculates the size of this chunk such that you could place that size in the
-- ChunkSize section of that chunk in a RIFF file that you wrote to disk.
calculateChunkLength 
   :: RiffChunk      -- ^ The RIFF chunk whose size should be calculated.
   -> RiffChunkSize  -- ^ The size of the data portion of that chunk no disk.
calculateChunkLength (RiffChunkChild _ chunkData) = fromIntegral $ BL.length chunkData
calculateChunkLength (RiffChunkParent _ children) = 
   idLength + childHeaderLength children + childrenLength children

idLength = 4
childHeaderLength children = fromIntegral (8 * length children)
childrenLength = sum . fmap (padToWord . calculateChunkLength)
