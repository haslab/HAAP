{-
HAAP: Haskell Automated Assessment Platform

This module provides documentation analysis functions by resorting to the _Haddock_ library (<https://hackage.haskell.org/package/haddock-library>).

-}


{-# LANGUAGE OverloadedStrings, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables #-}

module HAAP.Code.Analysis.Haddock where

import HAAP.Core
import HAAP.Utils
import HAAP.IO
import HAAP.Code.Haskell
import HAAP.Shelly

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Comments
import Documentation.Haddock.Parser
import Documentation.Haddock.Types
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc

import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Generics hiding (Generic)
import Data.List as List
import qualified Data.Text as Text
import Data.Default
import Data.Csv (header,DefaultOrdered(..),Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),namedRecord)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe

import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Monad.IO.Class

import System.FilePath

import Safe

import GHC.Generics (Generic)

#if MIN_VERSION_haddock_library(1,6,0)
deriving instance Data id => Data (Table id)
deriving instance Data id => Data (TableRow id)
deriving instance Data id => Data (TableCell id)
#endif

deriving instance Data Hyperlink
deriving instance Data Example
deriving instance (Data mod,Data id) => Data (DocH mod id)
deriving instance (Data x) => Data (Header x)
deriving instance Data Picture

data HaddockStats = HaddockStats
    { haddockComments :: Fraction -- (number of special annotations / total size of comments)
    , haddockCoverage :: Fraction -- (number of haddock comments / number of definitions)
    }
  deriving (Show,Generic)

instance NFData HaddockStats where

data Fraction = Fraction
    { numerador :: Int
    , denominador :: Int
    }
  deriving (Show,Generic)
  
instance NFData Fraction where
  
instance DefaultOrdered Fraction where
    headerOrder _ = header ["numerador", "denominador"]
instance DefaultOrdered HaddockStats where
    headerOrder _ = Vector.concat
        [addPrefixHeader "haddockComments" (headerOrder (undefined::Fraction))
        ,addPrefixHeader "haddockCoverage" (headerOrder (undefined::Fraction))
        ]
        
instance Default Fraction where
    def = Fraction (-1) (-1)

instance ToNamedRecord Fraction where
    toNamedRecord (Fraction x y) = namedRecord ["numerador" .= x,"denominador" .= y]
instance FromNamedRecord Fraction where
    parseNamedRecord m = Fraction <$> m .: "numerador" <*> m .: "denominador"

instance ToNamedRecord HaddockStats where
    toNamedRecord (HaddockStats x y) = HashMap.union
        (addPrefixNamedRecord "haddockComments" $ toNamedRecord x)
        (addPrefixNamedRecord "haddockCoverage" $ toNamedRecord y)
instance FromNamedRecord HaddockStats where
    parseNamedRecord m = do
        x <- parseNamedRecord (remPrefixNamedRecord "haddockComments" m)
        y <- parseNamedRecord (remPrefixNamedRecord "haddockCoverage" m)
        return $ HaddockStats x y

instance Default HaddockStats where
    def = HaddockStats def def

runHaddockStats :: (MonadIO m,HaapStack t m) => [FilePath] -> Haap t m HaddockStats
runHaddockStats files = do
    comments <- runHaddockComments files
    coverage <- runHaddockCoverage files
    return $ HaddockStats comments coverage

-- returns (number of special annotations,total size of comments)
runHaddockComments :: (MonadIO m,HaapStack t m) => [FilePath] -> Haap t m Fraction
runHaddockComments files = orLogDefault def $ do
    strs <- liftM (concat . catMaybes) $ mapM (orLogMaybe . parseFileComments) files
    let docs::[DocH Identifier Identifier] = map (_doc . parseParasGeneric) strs
    return $ Fraction (Set.size $ specialDocs docs) (sum $ map length strs)

#if MIN_VERSION_haddock_library(1,6,0)
parseParasGeneric = parseParas . Just
#else 
parseParasGeneric = parseParas
#endif

specialDoc :: DocH mod id -> Maybe Int
specialDoc (DocParagraph {}) = Just 1
specialDoc (DocIdentifier {}) = Just 2
specialDoc (DocIdentifierUnchecked {}) = Just 2
specialDoc (DocModule {}) = Just 3
specialDoc (DocWarning {}) = Just 4
specialDoc (DocEmphasis {}) = Just 5
specialDoc (DocMonospaced {}) = Just 6
specialDoc (DocBold {}) = Just 7
specialDoc (DocUnorderedList {}) = Just 8
specialDoc (DocOrderedList {}) = Just 9
specialDoc (DocDefList {}) = Just 10
specialDoc (DocCodeBlock {}) = Just 11
specialDoc (DocHyperlink {}) = Just 12
specialDoc (DocPic {}) = Just 13
specialDoc (DocMathInline {}) = Just 14
specialDoc (DocMathDisplay {}) = Just 15
specialDoc (DocAName {}) = Just 16
specialDoc (DocProperty {}) = Just 17
specialDoc (DocExamples {}) = Just 18
specialDoc (DocHeader {}) = Just 19
specialDoc d = Nothing

specialDocs :: Data a => a -> Set Int
specialDocs = everything Set.union (mkQ Set.empty aux)
    where
    aux :: DocH Identifier Identifier -> Set Int
    aux d = maybe Set.empty Set.singleton $ specialDoc d

commentString :: Comment -> String
commentString (Comment _ _ str) = str

parseFileComments :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m [String]
parseFileComments file = runBaseIO' $ do
    str <- readFile file
    case parseWithComments defaultParseMode str of
        ParseOk (_::Module SrcSpanInfo,comments) -> return $ map (commentString) comments
        ParseFailed _ _ -> return []

runHaddockCoverage :: (MonadIO m,HaapStack t m) => [FilePath] -> Haap t m Fraction
runHaddockCoverage files = orLogDefault def $ do
    ccs <- mapM (hadCoverage) files
    let ccs' = catMaybes ccs
    let (cc1,cc2) = List.foldr (\(v1,v2) (w1,w2) -> (v1+w1,v2+w2)) (0,0) ccs'
    return $ Fraction cc1 cc2

hadCoverage :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m (Maybe (Int,Int))
hadCoverage file = orLogMaybe $ do
    modname <- parseModuleFileName file
    res <- orIOResult $ runBaseSh $ do
        shCd $ takeDirectory file
        shCommand "haddock" [takeFileName file]
    let coverage = filterHad (lines $ Text.unpack $ resStdout res) modname
    liftIO $ evaluate $ force coverage

filterHad :: [String] -> String -> (Int,Int)
filterHad l s = x
    where
    g :: String -> (Int, Int)
    g a = (readNote "filterHad" $ atNote "filterHad" (words a) 2, readNote "filterHad" $ initNote "filterHad" $ atNote "filterHad" (words a) 4)
    x = headNote ("filterHad:"++show l ++"\n"++show s) $ List.map g $ List.filter (isInfixOf ("'" ++ s ++ "'")) l





