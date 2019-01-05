{-
HAAP: Haskell Automated Assessment Platform

This module provides documentation analysis functions by resorting to the _Haddock_ library (<https://hackage.haskell.org/package/haddock-library>).

-}


{-# LANGUAGE CPP, OverloadedStrings, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable, ScopedTypeVariables #-}

module HAAP.Code.Analysis.Haddock where

import HAAP.Core
import HAAP.Utils
import HAAP.IO
import HAAP.Code.Haskell
import HAAP.Shelly
import HAAP.Parse.IndentTree

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Comments
import Documentation.Haddock.Parser
import Documentation.Haddock.Types
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc

import Data.Char
import Data.Tree
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Generics hiding (Generic)
import Data.List as List
import Data.List.Split
import qualified Data.Text as Text
import Data.Default
import qualified Data.Csv
import Data.Csv (header,DefaultOrdered(..),Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),namedRecord)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.String

import Control.Monad
import Control.DeepSeq
--import Control.Exception
import Control.Monad.IO.Class
import qualified Control.Exception as E

import System.FilePath

import Safe

import Text.ParserCombinators.Parsec.Char as P
import Text.ParserCombinators.Parsec.Number as P
import Text.ParserCombinators.Parsec.Combinator as P
import Text.ParserCombinators.Parsec.Prim as P
import Text.ParserCombinators.Parsec.Error as P

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
	, haddockSpecials :: Map String Int -- number of occurences of each special annotation
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
		,addPrefixHeader "haddockSpecials" (Vector.fromList $ map fromString specialDocNames)
		]
       
instance Semigroup Fraction where
    (<>) = mappend
instance Monoid Fraction where
    mempty = Fraction 0 0
    mappend (Fraction x1 y1) (Fraction x2 y2) = Fraction (x1+x2) (y1+y2)
instance Default Fraction where
    def = Fraction (-1) (-1)

instance ToNamedRecord Fraction where
    toNamedRecord (Fraction x y) = namedRecord ["numerador" .= x,"denominador" .= y]
instance FromNamedRecord Fraction where
    parseNamedRecord m = Fraction <$> m .: "numerador" <*> m .: "denominador"

instance ToNamedRecord HaddockStats where
    toNamedRecord (HaddockStats x y z) = HashMap.unions
        [addPrefixNamedRecord "haddockComments" $ toNamedRecord x
        ,addPrefixNamedRecord "haddockCoverage" $ toNamedRecord y
		,addPrefixNamedRecord "haddockSpecials" $ toNamedRecord z
		]
instance FromNamedRecord HaddockStats where
    parseNamedRecord m = do
		x <- parseNamedRecord (remPrefixNamedRecord "haddockComments" m)
		y <- parseNamedRecord (remPrefixNamedRecord "haddockCoverage" m)
		z <- parseNamedRecord (remPrefixNamedRecord "haddockSpecials" m)
		return $ HaddockStats x y z

instance Default HaddockStats where
    def = HaddockStats def def defSpecials
    
defSpecials = Map.fromList $ zip specialDocNames $ repeat 0

runHaddockStats :: (MonadIO m,HaapStack t m) => [FilePath] -> Haap t m (HaddockStats,Map String (Set String))
runHaddockStats files = do
    (comments,specials) <- runHaddockComments files
    (coverage,missings) <- runHaddockCoverageFiles files
    return (HaddockStats comments coverage specials,missings)

-- returns (number of special annotations,total size of comments)
runHaddockComments :: (MonadIO m,HaapStack t m) => [FilePath] -> Haap t m (Fraction,Map String Int)
runHaddockComments files = orLogDefault def $ do
	strs <- liftM (concat . catMaybes) $ mapM (orLogMaybe . parseFileComments) files
	let docs::[DocH Identifier Identifier] = map (_doc . parseParasGeneric) strs
	let specials = specialDocs docs
	return (Fraction (Map.size $ specials) (sum $ map length strs),specials)

#if MIN_VERSION_haddock_library(1,6,0)
parseParasGeneric = parseParas Nothing
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
specialDoc (DocTable {}) = Just 20
specialDoc d = Nothing

specialDocName :: DocH mod id -> Maybe String
specialDocName (DocParagraph {}) = Just "paragraph"
specialDocName (DocIdentifier {}) = Just "identifier"
specialDocName (DocIdentifierUnchecked {}) = Just "identifier"
specialDocName (DocModule {}) = Just "module"
specialDocName (DocWarning {}) = Just "warning"
specialDocName (DocEmphasis {}) = Just "emphasis"
specialDocName (DocMonospaced {}) = Just "monospaced"
specialDocName (DocBold {}) = Just "bold"
specialDocName (DocUnorderedList {}) = Just "unorderedlist"
specialDocName (DocOrderedList {}) = Just "orderedlist"
specialDocName (DocDefList {}) = Just "deflist"
specialDocName (DocCodeBlock {}) = Just "codeblock"
specialDocName (DocHyperlink {}) = Just "hyperlink"
specialDocName (DocPic {}) = Just "pic"
specialDocName (DocMathInline {}) = Just "mathinline"
specialDocName (DocMathDisplay {}) = Just "mathdisplay"
specialDocName (DocAName {}) = Just "aname"
specialDocName (DocProperty {}) = Just "property"
specialDocName (DocExamples {}) = Just "examples"
specialDocName (DocHeader {}) = Just "header"
specialDocName (DocTable {}) = Just "table"
specialDocName d = Nothing

specialDocNames :: [String]
specialDocNames = 
	["paragraph"
	,"identifier"
	,"module"
	,"warning"
	,"emphasis"
	,"monospaced"
	,"bold"
	,"unorderedlist"
	,"orderedlist"
	,"deflist"
	,"codeblock"
	,"hyperlink"
	,"pic"
	,"mathinline"
	,"mathdisplay"
	,"aname"
	,"property"
	,"examples"
	,"header"
	,"table"
	]

specialDocs :: Data a => a -> Map String Int
specialDocs = everything (Map.unionWith (+)) (mkQ defSpecials aux)
    where
    aux :: DocH Identifier Identifier -> Map String Int
    aux d = case (specialDocName d,specialDoc d) of
		(Just dn,Just di) -> Map.singleton dn di
		otherwise -> defSpecials

commentString :: Comment -> String
commentString (Comment _ _ str) = str

parseFileComments :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m [String]
parseFileComments file = runBaseIO' $ do
    str <- readFile file
    case parseWithComments defaultParseMode str of
        ParseOk (_::Module SrcSpanInfo,comments) -> return $ map (commentString) comments
        ParseFailed _ _ -> return []

runHaddockCoverageFiles :: (MonadIO m,HaapStack t m) => [FilePath] -> Haap t m (Fraction,Map String (Set String))
runHaddockCoverageFiles files = orLogDefault def $ do
    ccs <- mapM runHaddockCoverageFile files
    let ccs' = catMaybes ccs
    let (cc1,cc2) = List.foldr mappend (mempty,Map.empty) ccs'
    return (cc1,cc2)

runHaddockCoverageFile :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m (Maybe (Fraction,Map String (Set String)))
runHaddockCoverageFile file = orLogMaybe $ do
    modname <- parseModuleFileName file
    let filename = takeFileName file
    res <- orIOResult' $ runBaseSh' $ do
        shCd $ takeDirectory file
        shCommand "haddock" [filename]
    let (coverage,missings) = parseHaddockOutput (Text.unpack $ resStdout res) filename modname
    liftIO $ E.evaluate $ force (coverage,Map.singleton modname missings)

parseHaddockOutput :: String -> FilePath -> String -> (Fraction,Set String)
parseHaddockOutput str filepath modname = if oks+length missings==total
    then res
    else error $ "parseHaddockOutput: " ++ show modname ++ "\n" ++ show forest ++ "\n" ++ show res
  where
    res@(Fraction oks total,missings) = parseForest forest
    Right forest = parseIndentedForest str
    
    parseForest :: Forest String -> (Fraction,Set String)
    parseForest (Node lbl ts:xs) = case parseCoverageLabel lbl of
        Right fr -> (fr,parseMissings xs)
        Left err -> parseForest ts `mappend` parseForest xs
    parseForest [] = (Fraction 0 0,Set.empty)
    
    parseMissings :: Forest String -> Set String
    parseMissings (Node "Missing documentation for:" ms:_) = parseMissings' ms 
    parseMissings xs = Set.empty
    parseMissings' :: Forest String -> Set String
    parseMissings' [] = Set.empty
    parseMissings' (Node x ts:xs) = case parseMissing x of
        Left err -> error $ "parseMissings' " ++ show filepath ++ " " ++ show x
        Right m -> Set.insert m $ parseMissings' xs
    
    parseCoverageLabel :: String -> Either P.ParseError Fraction
    parseCoverageLabel = P.parse parserCoverageLabel ""
    --100% ( 36 / 36) in 'Tarefa0_2018li1g088'
    parserCoverageLabel = do
        P.int
        P.char '%'
        P.spaces
        P.char '('
        P.spaces 
        x <- P.int
        P.spaces
        P.char '/'
        P.spaces
        y <- P.int
        P.spaces
        P.char ')'
        P.spaces
        P.string "in"
        P.spaces
        P.string "'"
        P.string modname
        P.string "'"
        P.spaces
        return $ Fraction x y
    
    parseMissing :: String -> Either P.ParseError String
    parseMissing = P.parse parserMissing ""
    --t2_1 (Tarefa0_2018li1g088.hs:17)
    parserMissing = do
        funname <- P.many1 $ P.satisfy (not . isSpace)
        P.spaces
        P.char '('
        P.string filepath
        P.char ':'
        line <- P.int
        P.char ')'
        P.spaces
        return funname







