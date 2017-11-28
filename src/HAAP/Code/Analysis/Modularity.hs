{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module HAAP.Code.Analysis.Modularity where

import HAAP.Core
import HAAP.IO
import HAAP.Code.Haskell

import Data.List as List
import Data.Generics hiding (Generic)
import Data.Traversable
import Data.Either
import Data.Maybe
import Data.Default
import Data.Csv (header,DefaultOrdered(..),Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),namedRecord)

import Control.Monad

import Language.Haskell.Exts

import GHC.Generics (Generic)

data Modularity = Modularity
    { blockSize :: Float -- average block size
    , totalSize :: Int -- total size
    }
    
instance DefaultOrdered Modularity where
    headerOrder _ = header ["blockSize","totalSize"]

instance ToNamedRecord Modularity where
    toNamedRecord (Modularity x y) = namedRecord ["blockSize" .= x,"totalSize" .= y]
instance FromNamedRecord Modularity where
    parseNamedRecord m = Modularity <$> m .: "blockSize" <*> m .: "totalSize"

instance Default Modularity where
    def = Modularity (-1) (-1)

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