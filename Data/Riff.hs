{-|
Description : Convenience module that brings together both the parsing and assebling of Riff files.
Copyright   : (c) Robert Massaioli, 2014
License     : MIT
Maintainer  : robertmassaioli@gmail.com
Stability   : experimental

This module was made as a convinience, it's purpose is to aid the manipulation of RIFF files such 
that you can both parse them and asseble them. If you wish to just parse or assemble Riff files 
then you may be better off just importing Data.Riff.Parse or Data.Riff.Assemble respectively.
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
   parseRiffData
   ) where

import Data.Riff.RiffData
import Data.Riff.Parse
