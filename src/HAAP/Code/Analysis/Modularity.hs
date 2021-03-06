{-
HAAP: Haskell Automated Assessment Platform

This module extracts basic modularity metrics from Haskell code.

-}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module HAAP.Code.Analysis.Modularity where

import HAAP.Core
import HAAP.IO
import HAAP.Code.Haskell
import HAAP.Log
import HAAP.Pretty

import qualified Data.Text as T
import Data.List as List
import Data.Generics hiding (Generic)
import Data.Traversable
import Data.Either
import Data.Maybe
import Data.Default
import Data.Csv (header,DefaultOrdered(..),Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),namedRecord)

import Control.Monad
import Control.DeepSeq
import Control.Monad.IO.Class

import Language.Haskell.Exts

import GHC.Generics (Generic)

data Modularity = Modularity
    { blockSize :: Float -- average block size
    , totalSize :: Int -- total size
    }
  deriving (Show,Generic)
instance NFData Modularity where
    
instance DefaultOrdered Modularity where
    headerOrder _ = header ["blockSize","totalSize"]

instance ToNamedRecord Modularity where
    toNamedRecord (Modularity x y) = namedRecord ["blockSize" .= x,"totalSize" .= y]
instance FromNamedRecord Modularity where
    parseNamedRecord m = Modularity <$> m .: "blockSize" <*> m .: "totalSize"

instance Default Modularity where
    def = Modularity (-1) (-1)

runModularity :: (MonadIO m,HaapStack t m) => [FilePath] -> Haap t m Modularity
runModularity files = do
    modus <- liftM catMaybes $ forM files $ \file -> orLogMaybe $ do
        logEvent $ "parsing modularity for " <> prettyText file
        parseHaskellFile file
    let sizes = sort $ concat $ List.map (maxSize) modus
    let sizesf = drop (length sizes - 5) sizes
    let size = if (length sizes == 0) then (-1) else (fromIntegral $ sum $ sizesf) / (fromIntegral $ length sizesf)
    let tsizes = List.map (gsize) $ modus
    return $ Modularity size (sum tsizes)
  where
    maxSize :: Module SrcSpanInfo -> [Int]
    maxSize m = sizes
        where sizes = List.map gsize (List.map noLocDecl $ removeLets $ functions $ removeTopConstants $ getTopDecls m)