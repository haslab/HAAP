module HAAP.Code.Analysis.Modularity where

import HAAP.Core
import HAAP.IO
import HAAP.Code.Haskell

import Data.List as List
import Data.Generics
import Data.Traversable
import Data.Either
import Data.Maybe

import Control.Monad

import Language.Haskell.Exts

data Modularity = Modularity
    { blockSize :: Float -- average block size
    , totalSize :: Int -- total size
    }

runModularity :: HaapMonad m => [FilePath] -> Haap p args db m Modularity
runModularity files = do
    modus <- forM files $ \file -> orMaybe $ parseHaskellFile file
    let sizes = sort $ concat $ List.map (maybe [] maxSize) modus
    let sizesf = drop (length sizes - 5) sizes
    let size = if (length sizes == 0) then (-1) else (fromIntegral $ sum $ sizesf) / (fromIntegral $ length sizesf)
    let tsizes = List.map (gsize) $ catMaybes modus
    return $ Modularity size (sum tsizes)
  where
    maxSize :: Module SrcSpanInfo -> [Int]
    maxSize m = sizes
        where sizes = List.map gsize (removeLets $ List.map noLocDecl $ functions $ removeTopConstants $ getTopDecls m)