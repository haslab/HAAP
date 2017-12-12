{-# LANGUAGE ViewPatterns, DeriveGeneric, OverloadedStrings #-}

module HAAP.Code.HPC where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils
import HAAP.Compiler.GHC
import HAAP.Web.HTML.TagSoup
import HAAP.Pretty

import Data.Traversable
import Data.Foldable
import Data.Maybe
import Data.List
import Data.Default
import Data.List.Split
import qualified Data.Text as Text
import Data.Csv (header,DefaultOrdered(..),Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),namedRecord)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

import Text.HTML.TagSoup

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import qualified Control.Monad.Reader as Reader

import System.FilePath

import Safe

import GHC.Generics (Generic)

data HpcArgs args = HpcArgs
    { hpcExecutable :: FilePath -- executables to run with hpc
    , hpcGHC :: args -> GHCArgs
    , hpcIO :: args -> IOArgs
    , hpcSandbox :: Maybe FilePath
    , hpcHtmlPath :: Maybe FilePath -- relative path to the project to store hpc results
    , hpcRTS :: Bool
    }

data HpcItem = HpcItem
    { hpcPercentage :: Int
    , hpcUsed :: Int
    , hpcTotal :: Int
    }
  deriving (Generic,Show)
  
instance NFData HpcItem
    
instance Default HpcItem where
    def = HpcItem (-1) (-1) (-1)
    
instance DefaultOrdered HpcItem where
    headerOrder _ = header ["hpcPercentage","hpcUsed","hpcTotal"]

instance ToNamedRecord HpcItem where
    toNamedRecord (HpcItem x y z) = namedRecord ["hpcPercentage" .= x,"hpcUsed" .= y,"hpcTotal" .= z]
instance FromNamedRecord HpcItem where
    parseNamedRecord m = HpcItem <$> m .: "hpcPercentage" <*> m .: "hpcUsed" <*> m .: "hpcTotal"

    
data HpcReport = HpcReport
    { hpcExpressions :: HpcItem -- expressions used
    , hpcBoolean :: HpcItem -- boolean coverage
    , hpcAlternatives :: HpcItem -- alternatives used
    , hpcLocalDeclarations :: HpcItem -- local declarations used
    , hpcTopDeclarations :: HpcItem -- top-level declarations used
    }
  deriving (Generic,Show)

instance NFData HpcReport

instance Default HpcReport where
    def = HpcReport def def def def def

instance DefaultOrdered HpcReport where
    headerOrder _ = Vector.concat
        [addPrefixHeader "hpcExpressions" (headerOrder (undefined::HpcItem))
        ,addPrefixHeader "hpcBoolean" (headerOrder (undefined::HpcItem))
        ,addPrefixHeader "hpcAlternatives" (headerOrder (undefined::HpcItem))
        ,addPrefixHeader "hpcLocalDeclarations" (headerOrder (undefined::HpcItem))
        ,addPrefixHeader "hpcTopDeclarations" (headerOrder (undefined::HpcItem))
        ]

instance ToNamedRecord HpcReport where
    toNamedRecord (HpcReport x1 x2 x3 x4 x5) = HashMap.unions
        [(addPrefixNamedRecord "hpcExpressions" $ toNamedRecord x1)
        ,(addPrefixNamedRecord "hpcBoolean" $ toNamedRecord x2)
        ,(addPrefixNamedRecord "hpcAlternatives" $ toNamedRecord x3)
        ,(addPrefixNamedRecord "hpcLocalDeclarations" $ toNamedRecord x4)
        ,(addPrefixNamedRecord "hpcTopDeclarations" $ toNamedRecord x5)
        ]
instance FromNamedRecord HpcReport where
    parseNamedRecord m = do
        x1 <- parseNamedRecord (remPrefixNamedRecord "hpcExpressions" m)
        x2 <- parseNamedRecord (remPrefixNamedRecord "hpcBoolean" m)
        x3 <- parseNamedRecord (remPrefixNamedRecord "hpcAlternatives" m)
        x4 <- parseNamedRecord (remPrefixNamedRecord "hpcLocalDeclarations" m)
        x5 <- parseNamedRecord (remPrefixNamedRecord "hpcTopDeclarations" m)
        return $ HpcReport x1 x2 x3 x4 x5

hpcCleanup :: HaapMonad m => FilePath -> FilePath -> Haap p args db m ()
hpcCleanup dir exec = do
    ignoreError $ runSh $ do
        shCd dir
        shRm ".hpc"
        shFindGlob "*.tix" >>= mapM_ shRm
        shFindGlob "*.o" >>= mapM_ shRm
        shFindGlob "*.hi" >>= mapM_ shRm


runHpcReport :: HpcArgs args -> a -> (IOResult -> Haap p args db IO a) -> Haap p args db IO (a,HpcReport)
runHpcReport hpc defa m = orLogDefault (defa,def) $ do
    tmp <- getProjectTmpPath
    ghc <- Reader.reader (hpcGHC hpc)
    let ghc' = ghc { ghcHpc = True, ghcRTS = hpcRTS hpc }
    io <- Reader.reader (hpcIO hpc)
    let io' = io { ioSandbox = fmap (dirToRoot dir </>) (hpcSandbox hpc) }
    do
        hpcCleanup dir exec
            
        ghcres <- runShIOResult $ do
            shCd dir
            res <- shGhcWith io' ghc' [exec]
            return res
            
        x <- m ghcres
        
        hpcres <- runSh $ do
            shCd dir
            shCommandWith io' "hpc" ["report",exec]
        addMessageToError (pretty hpcres) $ do
            let xs = map words $ lines $ Text.unpack $ resStdout hpcres     
            report <- orLogDefault def $ liftIO $ evaluate $ force $ HpcReport (parseHpcItem xs 0) (parseHpcItem xs 1) (parseHpcItem xs 5) (parseHpcItem xs 6) (parseHpcItem xs 7)
            return (x,report)
         
  where
    parseHpcItem xs i = case (atNote "parseHpcItem" xs i) of
        (percentage:(last -> fraction)) -> case tail (init fraction) of
            (splitOn "/" -> [l,r]) -> HpcItem
                (readNote "read percentage" $ init percentage)
                (readNote "read fraction l" l)
                (readNote "read fraction r" r)
            frac -> error $ "hpc fraction " ++ show frac
        line -> error $ "hpc line " ++ show line
    (dir,exec) = splitFileName (hpcExecutable hpc)

runHpc :: Out a => HakyllP -> HpcArgs args -> a -> (IOResult -> Haap p args db Hakyll a) -> Haap p args db Hakyll (a,FilePath)
runHpc hp hpc def m = orErrorHakyllPage hp outhtml (def,outhtml) $ do
    tmp <- getProjectTmpPath
    ghc <- Reader.reader (hpcGHC hpc)
    let ghc' = ghc { ghcHpc = True, ghcRTS = hpcRTS hpc }
    io <- Reader.reader (hpcIO hpc)
    let io' = io { ioSandbox = fmap (dirToRoot dir </>) (hpcSandbox hpc) }
    do
        hpcCleanup dir exec
            
        ghcres <- runShIOResult $ do
            shCd dir
            res <- shGhcWith io' ghc' [exec]
            return res
            
        x <- m ghcres
        
        if (isJust $ hpcHtmlPath hpc)
            then do
                let destdir = dirToRoot dir </> tmp </> maybe "" id (hpcHtmlPath hpc) </> exec
                orErrorHakyllPage hp outhtml () $ do
                    orErrorWritePage (tmp </> html) mempty $ runSh $ do
                        shCd dir
                        shCommandWith io' "hpc" ["markup",exec,"--destdir="++destdir]
                    
                    hakyllRules $ do
                        -- copy the hpc generated documentation
                        match (fromGlob $ tmp </> fromJust (hpcHtmlPath hpc) </> exec </> "*") $ do
                            route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                            compile $ do
                                file <- getResourceFilePath
                                getResourceString >>= liftCompiler (asTagSoupHTML $ addLegend file . tagSoupChangeLinkUrls (hakyllRoute hp)) >>= hakyllCompile hp
                return (x,outhtml)
            else return (x,"")
  where
    (dir,exec) = splitFileName (hpcExecutable hpc)
    html = maybe "" id (hpcHtmlPath hpc) </> exec </> "hpc_index.html"
    outhtml = hakyllRoute hp $ html

addLegend :: FilePath -> TagHtml -> TagHtml
addLegend file html = if isInfixOf ".hs" file
    then injectHTMLBody hpcLegend html
    else html

hpcLegend :: TagHtml
hpcLegend = parseTags 
    "<pre>\n<span class=\"decl\"><span class=\"nottickedoff\">never executed</span> <span class=\"tickonlytrue\">always true</span> <span class=\"tickonlyfalse\">always false</span></span>\n</pre>"


