{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Showing references to slices of code
module Language.Haskell.Homplexity.SrcSlice (
    SrcSlice
  , srcSlice
  , srcLoc
  , showSrcSpan
  , mergeSrcLocs
  , sliceFirstLine
  , sliceLastLine
  , sliceFilename
  , locAsSpan
  ) where

import Data.Data
import Data.Generics.Uniplate.Data
import Control.Arrow
import Control.Exception (assert)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc

-- * Slice of code
type SrcSlice  = SrcSpan

sliceFilename :: SrcSpan -> String
sliceFilename  = srcSpanFilename

sliceFirstLine :: SrcSpan -> Int
sliceFirstLine = srcSpanStartLine

sliceLastLine :: SrcSpan -> Int
sliceLastLine  = srcSpanEndLine

srcLoc :: (Data code, Show code) => code -> SrcLoc
srcLoc code = checkHead  $
              universeBi   code
  where
    msg             = "Cannot find SrcLoc in the code fragment: " ++ show code
    checkHead []    = error msg
    checkHead (e:_) = e

-- | Compute the slice of code that given source fragment is in (for naming)
srcSlice     :: (Data a, Show a)
             => a -> SrcSpan
srcSlice code = mergeSrcLocs
              . checkNonEmpty
              . universeBi    $ code
  where
    checkNonEmpty []    = error $ "Can't know how make a SrcSlice from code fragment: " ++ show code
    checkNonEmpty other = other

mergeSrcLocs :: [SrcLoc] -> SrcSpan
mergeSrcLocs []        = error "Don't know how make a SrcSpan from an empty list of locations!"
mergeSrcLocs sliceLocs = allEqual (map srcFilename sliceLocs) `assert`
                           SrcSpan {..}
  where
    srcSpanFilename = srcFilename $ head sliceLocs
    ((srcSpanStartLine, srcSpanStartColumn),
     (srcSpanEndLine,   srcSpanEndColumn  )) = (minimum &&& maximum) $
                                               map (srcLine &&& srcColumn) sliceLocs

locAsSpan              :: SrcLoc -> SrcSpan
locAsSpan (SrcLoc {..}) = SrcSpan { srcSpanStartLine   = srcLine
                                  , srcSpanEndLine     = srcLine
                                  , srcSpanStartColumn = srcColumn
                                  , srcSpanEndColumn   = srcColumn
                                  , srcSpanFilename    = srcFilename
                                  }

allEqual       ::  Eq a => [a] -> Bool
allEqual []     = True
allEqual (b:bs) = all (b==) bs

showSrcSpan               :: SrcSpan -> ShowS
showSrcSpan (SrcSpan {..}) = shows srcSpanFilename
                           . (':':)
                           . shows srcSpanStartLine
                           . ('-':)
                           . shows srcSpanEndLine

