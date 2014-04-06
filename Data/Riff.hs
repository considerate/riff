{-|
Description : The module that allows parsing of a RIFF / RIFX file.
Copyright   : (c) Robert Massaioli, 2014
License     : MIT
Maintainer  : robertmassaioli@gmail.com
Stability   : experimental

This module allows the parsing of RIFF files in pure Haskell. You can parse a RIFF file using the 
methods provided in this module. For example, if you wanted to parse a RIFF file and print it out
then you could:

> main = withRiffFile "path/to/file.riff" print

And that will print the RIFF file, in gory details, to the screen.
-}
module Data.Riff
   ( RiffFile(..)
   , RiffChunkSize
   , RiffFileType(..)
   , RiffChunk(..)
   , RiffId
   , RiffData
   , ParseError
   , withRiffFile
   , parseRiffData
   ) where

import Data.Riff.RiffData
import Data.Riff.Parse
