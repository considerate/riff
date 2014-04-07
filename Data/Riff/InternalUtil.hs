module Data.Riff.InternalUtil where

import Data.Riff.RiffData

padToWord :: RiffChunkSize -> RiffChunkSize
padToWord x = if x `mod` 2 == 0
   then x
   else x + 1
