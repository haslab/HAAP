{-# LANGUAGE OverloadedStrings, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables #-}

module HAAP.Code.Analysis.Haddock where

import HAAP.Core
import HAAP.Utils
import HAAP.IO
import HAAP.Code.Haskell

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
import Data.Csv (Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),namedRecord)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

import Control.Monad

import System.FilePath

import GHC.Generics (Generic)

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

data Fraction = Fraction
    { numerador :: Int
    , denominador :: Int
    }
  deriving (Show,Generic)

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

runHaddockStats :: HaapMonad m => [FilePath] -> Haap p args db m HaddockStats
runHaddockStats files = do
    comments <- runHaddockComments files
    coverage <- runHaddockCoverage files
    return $ HaddockStats comments coverage

-- returns (number of special annotations,total size of comments)
runHaddockComments :: HaapMonad m => [FilePath] -> Haap p args db m Fraction
runHaddockComments files = do
    strs <- liftM concat $ mapM (parseFileComments) files
    let docs::[DocH Identifier Identifier] = map (_doc . parseParas) strs
    return $ Fraction (Set.size $ specialDocs docs) (sum $ map length strs)

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

parseFileComments :: HaapMonad m => FilePath -> Haap p args db m [String]
parseFileComments file = orDefault [] $ runIO' $ do
    str <- readFile file
    case parseWithComments defaultParseMode str of
        ParseOk (_::Module SrcSpanInfo,comments) -> return $ map (commentString) comments
        ParseFailed _ _ -> return []

runHaddockCoverage :: HaapMonad m => [FilePath] -> Haap p args db m Fraction
runHaddockCoverage files = do
    ccs <- mapM hadCoverage files
    let (cc1,cc2) = List.foldr (\(v1,v2) (w1,w2) -> (v1+w1,v2+w2)) (0,0) ccs
    return $ Fraction cc1 cc2

hadCoverage :: HaapMonad m => FilePath -> Haap p args db m (Int,Int)
hadCoverage file = orDefault (0,0) $ do
    modname <- orDefault (takeBaseName file) $ parseModuleFileName file
    res <- runShIOResult $ do
        shCd $ takeDirectory file
        shCommand "haddock" [takeFileName file]
    let coverage = filterHad (lines $ Text.unpack $ resStdout res) modname
    return coverage

filterHad :: [String] -> String -> (Int,Int)
filterHad l s = x
    where
    g :: String -> (Int, Int)
    g a = (read $ (words a) !! 2, read $ init $ (words a) !! 4)
    x = head $ List.map g $ List.filter (isInfixOf ("'" ++ s ++ "'")) l





