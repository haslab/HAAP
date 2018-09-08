{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Brittany@ plugin that invokes the external _brittany_ tool (<https://hackage.haskell.org/package/brittany>) to automatically indent Haskell code.

-}

{-# LANGUAGE EmptyDataDecls, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}

module HAAP.Code.Brittany where

import HAAP.Web.Diff
import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils
import HAAP.Plugin
import HAAP.Shelly
import HAAP.Pretty

import Data.Default
import Data.List
import qualified Data.Text as T
import Data.Proxy

import Control.Monad.Reader as Reader

import System.FilePath

data Brittany

instance HaapPlugin Brittany where
    type PluginI Brittany = BrittanyArgs
    type PluginO Brittany = ()
    type PluginT Brittany = ReaderT BrittanyArgs
    type PluginK Brittany t m = ()

    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . unComposeT) m
        return (x,())

instance HaapMonad m => HasPlugin Brittany (ReaderT BrittanyArgs) m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT (ReaderT BrittanyArgs) m (t2 m)) => HasPlugin Brittany (ComposeT (ReaderT BrittanyArgs) t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m

data BrittanyArgs = BrittanyArgs
    { brittanySandbox :: Sandbox
    , brittanyArgs :: [String]
    , brittanyPath :: FilePath -- path relative to the project where to execute the brittany command
    , brittanyFiles :: [FilePath] -- relative to the path where brittany is executed
    , brittanyHtmlPath :: FilePath -- relative to the project path
    }

useAndRunBrittany :: (MonadIO m,HasPlugin Hakyll t m) => BrittanyArgs -> Haap t m FilePath
useAndRunBrittany args = usePlugin_ (return args) $ runBrittany

runBrittany :: (MonadIO m,HasPlugin Hakyll t m,HasPlugin Brittany t m) => Haap t m FilePath
runBrittany = do
    h <- liftHaap $ liftPluginProxy (Proxy::Proxy Brittany) $ Reader.ask
    hp <- getHakyllP
    
    tmp <- getProjectTmpPath
    let ioArgs = def { ioSandbox = mapSandboxCfg (dirToRoot (brittanyPath h) </>) (brittanySandbox h) }
    let extras = brittanyArgs h
    let files = brittanyFiles h
    difffiles <- forM files $ \infile -> do
        instr <- orDo' (return . prettyText) $ runBaseSh $ do
            shCd $ brittanyPath h
            shReadFile infile
        outres <- orIOResult $ runBaseSh $ do
            shCd $ brittanyPath h
            shCommandWith ioArgs "brittany" [infile]
        let outstr = resStdout outres <> resStderr outres
        return (T.pack infile,instr,outstr)
    
    let title = "Brittany Code Formatter Tool"
    runDiff (DiffArgs title difffiles (brittanyHtmlPath h))
      
