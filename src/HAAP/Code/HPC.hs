{-
HAAP: Haskell Automated Assessment Platform

This module provides the @HPC@ plugin that invokes the external _hpc_ tool (<https://hackage.haskell.org/package/hpc>) for analyzing Haaskell code coverage.

-}


{-# LANGUAGE TypeOperators, EmptyDataDecls, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, ViewPatterns, DeriveGeneric, OverloadedStrings #-}

module HAAP.Code.HPC where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils
import HAAP.Compiler.GHC
import HAAP.Web.HTML.TagSoup
import HAAP.Pretty
import HAAP.Plugin
import HAAP.Shelly

import Data.Traversable
import Data.Foldable
import Data.Maybe
import Data.List
import Data.Default
import Data.List.Split
import qualified Data.Text as T
import Data.Csv (header,DefaultOrdered(..),Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),namedRecord)
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy

import Text.HTML.TagSoup

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Exception as E
import Control.Monad.Reader as Reader

import System.FilePath

import Safe

import GHC.Generics (Generic)

data HPC

instance HaapPlugin HPC where
    type PluginI HPC = HpcArgs
    type PluginO HPC = ()
    type PluginT HPC = ReaderT HpcArgs
    type PluginK HPC t m = ()
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . getComposeT) m
        return (x,())
    
instance HaapMonad m => HasPlugin HPC (ReaderT HpcArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin HPC (ComposeT (ReaderT HpcArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m
    
data HpcArgs = HpcArgs
    { hpcExecutable :: FilePath -- executables to run with hpc
    , hpcGHC :: GHCArgs
    , hpcIO :: IOArgs
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

hpcCleanup :: (MonadIO m,HasPlugin HPC t m) => FilePath -> FilePath -> Haap t m ()
hpcCleanup dir exec = do
    ignoreError $ runBaseSh $ do
        shCd dir
        shRm ".hpc"
        shFindGlob "." "*.tix" >>= mapM_ shRm
        shFindGlob "." "*.o" >>= mapM_ shRm
        shFindGlob "." "*.hi" >>= mapM_ shRm

useAndRunHpcReport :: (MonadIO m,HaapStack t m) => HpcArgs -> a -> (IOResult -> Haap (ReaderT HpcArgs :..: t) m a) -> Haap t m (a,HpcReport)
useAndRunHpcReport args x m = usePlugin_ (return args) $ runHpcReport x m

runHpcReport :: (MonadIO m,HasPlugin HPC t m) => a -> (IOResult -> Haap t m a) -> Haap t m (a,HpcReport)
runHpcReport defa m = do
    hpc <- liftHaap $ liftPluginProxy (Proxy::Proxy HPC) $ Reader.ask
    let parseHpcItem xs i = case (atNote "parseHpcItem" xs i) of
                                (percentage:(last -> fraction)) -> case tail (init fraction) of
                                    (splitOn "/" -> [l,r]) -> HpcItem
                                        (readNote "read percentage" $ init percentage)
                                        (readNote "read fraction l" l)
                                        (readNote "read fraction r" r)
                                    frac -> error $ "hpc fraction " ++ show frac
                                line -> error $ "hpc line " ++ show line
    let (dir,exec) = splitFileName (hpcExecutable hpc)
    orLogDefault (defa,def) $ do
        tmp <- getProjectTmpPath
        let ghc = (hpcGHC hpc)
        let io = (hpcIO hpc)
        let io' = io
        let ghc' = ghc { ghcMake = True, ghcHpc = True, ghcRTS = hpcRTS hpc, ghcIO = io' }
        do
            hpcCleanup dir exec
                
            ghcres <- orIOResult $ runBaseSh $ do
                shCd dir
                res <- shGhcWith ghc' [exec]
                return res
                
            x <- m ghcres
            
            hpcres <- runBaseSh $ do
                shCd dir
                shCommandWith io' "hpc" ["report",exec]
            addMessageToError (prettyText hpcres) $ do
                let xs = map words $ lines $ T.unpack $ resStdout hpcres     
                report <- orLogDefault def $ liftIO $ E.evaluate $ force $ HpcReport (parseHpcItem xs 0) (parseHpcItem xs 1) (parseHpcItem xs 5) (parseHpcItem xs 6) (parseHpcItem xs 7)
                return (x,report)

useAndRunHpc :: (MonadIO m,HasPlugin Hakyll t m,Pretty a) => HpcArgs -> a -> (IOResult -> Haap (ReaderT HpcArgs :..: t) m a) -> Haap t m (a,Maybe FilePath)
useAndRunHpc args x m = usePlugin_ (return args) $ runHpc x m

runHpc :: (MonadIO m,HasPlugin Hakyll t m,HasPlugin HPC t m,Pretty a) => a -> (IOResult -> Haap t m a) -> Haap t m (a,Maybe FilePath)
runHpc def m = do
    hpc <- liftHaap $ liftPluginProxy (Proxy::Proxy HPC) $ Reader.ask
    hp <- getHakyllP
    let (dir,exec) = splitFileName (hpcExecutable hpc)
    let html = fmap (</> exec </> "hpc_index.html") (hpcHtmlPath hpc) 
    let outhtml = fmap (hakyllRoute hp) html
    let onError = maybe id (\o -> orErrorHakyllPage o (def,Just o)) outhtml
    onError $ do
        tmp <- getProjectTmpPath
        let ghc = (hpcGHC hpc)
        let io = (hpcIO hpc)
        let io' = io 
        let ghc' = ghc { ghcMake = True, ghcHpc = isJust (hpcHtmlPath hpc), ghcRTS = hpcRTS hpc, ghcIO = io' }
        do
            hpcCleanup dir exec
                
            ghcres <- orIOResult $ runBaseSh $ do
                shCd dir
                res <- shGhcWith ghc' [exec]
                return res
                
            x <- m ghcres
            
            if (isJust $ hpcHtmlPath hpc)
                then do
                    let destdir = dirToRoot dir </> tmp </> maybe "" id (hpcHtmlPath hpc) </> exec
                    orErrorHakyllPage (fromJust outhtml) () $ do
                        orErrorWritePage (tmp </> fromJust html) mempty $ runBaseSh $ do
                            shCd dir
                            shCommandWith io' "hpc" ["markup",exec,"--destdir="++destdir]
                        
                        hakyllFocus [tmp </> fromJust (hpcHtmlPath hpc) </> exec] $ hakyllRules $ do
                            -- copy the hpc generated documentation
                            match (fromGlob $ tmp </> fromJust (hpcHtmlPath hpc) </> exec </> "*") $ do
                                route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                                compile $ do
                                    file <- getResourceFilePath
                                    getResourceString >>= liftCompiler (asTagSoupHTML $ addLegend file . tagSoupChangeLinkUrls (hakyllRoute hp)) >>= hakyllCompile hp
                    return (x,outhtml)
                else return (x,outhtml)

addLegend :: FilePath -> TagHtml -> TagHtml
addLegend file html = if isInfixOf ".hs" file
    then injectHTMLBody hpcLegend html
    else html

hpcLegend :: TagHtml
hpcLegend = parseTags 
    "<pre>\n<span class=\"decl\"><span class=\"nottickedoff\">never executed</span> <span class=\"tickonlytrue\">always true</span> <span class=\"tickonlyfalse\">always false</span></span>\n</pre>"


