module Data.Riff.Assemble where

import Data.Riff.RiffData

import Data.Binary.Put
import Data.Char (ord)

writeRiffFile :: RiffFile -> Put
writeRiffFile riffFile = do
   putString "RIFF"  

putString :: String -> Put
putString = sequence_ . fmap (putWord8 . fromIntegral . ord)


