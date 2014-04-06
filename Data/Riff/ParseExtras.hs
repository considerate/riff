module Data.Riff.ParseExtras
   ( getNChars
   , getNWords
   , getIdentifier
   ) where

import Data.Binary.Get
import Data.Char (chr)
import Data.Word
import Control.Monad (replicateM)

getNWords :: Int -> Get [Word8]
getNWords n = replicateM n getWord8

getNChars :: Int -> Get String
getNChars = fmap (fmap byteToChar) . getNWords 

-- Parsing bytes
byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

getIdentifier = getNChars 4
