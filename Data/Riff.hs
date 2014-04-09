{-|
Description : Convenience module that brings together both the parsing and assebling of Riff files.
Copyright   : (c) Robert Massaioli, 2014
License     : MIT
Maintainer  : robertmassaioli@gmail.com
Stability   : experimental

The RIFF module allows the parsing of RIFF files in pure Haskell. It was written to be as efficient
and as simple as possible.

You can parse a RIFF file using the methods provided in this module. For example, if you wanted 
to parse a RIFF file and print it out then you could:

> main = withRiffFile "path/to/file.riff" print

And that will print the RIFF file, in gory details, to the screen. You can also use this module to
create a RIFF file in pure Haskell starting with a RiffFile and building it up until you are ready 
to write it out to a ByteString or Disk. For example, this is how you might construct a RIFF file 
to be written out:

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
module Data.Riff ( 
   -- * RIFF File Data Representaion
   RiffFile(..),
   RiffChunkSize, 
   RiffFileType(..), 
   RiffChunk(..), 
   RiffId, 
   RiffData, 
   ParseError,
   -- * Reading (parsing) RIFF Files
   withRiffFile,
   parseRiffData,
   -- * Writing (assembling) RIFF Files
   assembleRiffFile,
   assembleRiffFileStream
   ) where

import Data.Riff.RiffData
import Data.Riff.Parse
import Data.Riff.Assemble
