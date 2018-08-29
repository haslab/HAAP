{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Debugger@ plugin that invokes external debugging tools.

-}

{-# LANGUAGE EmptyDataDecls, ScopedTypeVariables, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}

module HAAP.Code.Debugger where

import HAAP.Web.Diff
import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils
import HAAP.Plugin
import HAAP.Shelly
import HAAP.Pretty
import HAAP.Compiler.GHC

import Data.Default
import Data.List
import qualified Data.Text as Text
import Data.Proxy

import Control.Monad.Reader as Reader
import Control.Monad.Except

import System.FilePath
import System.Directory

import Debug.Hoed.Extras

data Debugger

instance HaapPlugin Debugger where
    type PluginI Debugger = DebuggerArgs
    type PluginO Debugger = ()
    type PluginT Debugger = ReaderT DebuggerArgs
    type PluginK Debugger t m = ()

    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . unComposeT) m
        return (x,())

instance HaapMonad m => HasPlugin Debugger (ReaderT DebuggerArgs) m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT (ReaderT DebuggerArgs) m (t2 m)) => HasPlugin Debugger (ComposeT (ReaderT DebuggerArgs) t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m

data DebuggerArgs = DebuggerArgs
    { debuggerSandbox :: Sandbox
    , debuggerArgs :: [String]
    , debuggerGHC :: GHCArgs
    , debuggerPath :: FilePath -- path relative to the project whose files are being debugged
    , debuggerFiles :: [FilePath] -- debugged files relative to the debugger path
    , debuggerInstrumentedPath :: FilePath -- path relative to the project path of lareaady instrumented debug files
    , debuggerInstrumentedFiles :: [FilePath] -- debug files relative to the instrumented path
    , debuggerModules :: [String]
    , debuggerProgram :: String
    , debuggerHtmlPath :: FilePath
    }

useAndRunDebugger :: (MonadIO m,HasPlugin Hakyll t m) => DebuggerArgs -> Haap t m (FilePath,FilePath,FilePath)
useAndRunDebugger args = usePlugin_ (return args) $ runDebugger

runDebugger :: (MonadIO m,HasPlugin Hakyll t m,HasPlugin Debugger t m) => Haap t m (FilePath,FilePath,FilePath)
runDebugger = do
    h <- liftHaap $ liftPluginProxy (Proxy::Proxy Debugger) $ Reader.ask
    hp <- getHakyllP
    tmp <- getProjectTmpPath
    let htmldatapath::String = dirToRoot (debuggerHtmlPath h) </> "debug"
    
    let extras = debuggerArgs h
    let ioargs = (ghcIO $ debuggerGHC h) { ioSandbox = debuggerSandbox h }
    let ioArgs = (ghcIO $ debuggerGHC h) { ioSandbox = mapSandboxCfg (dirToRoot (tmp </> debuggerHtmlPath h) </>) (debuggerSandbox h) }
    let ghcArgs = (debuggerGHC h) { ghcIO = ioArgs, ghcSafe = False }
    
    let debuggererrorpath = addExtension (debuggerHtmlPath h) "html"
    orErrorHakyllPage debuggererrorpath (debuggererrorpath,debuggererrorpath,debuggererrorpath) $ do
        --let html = dirToRoot (debuggerPath h) </> tmp </> debuggerHtmlPath h
        runBaseSh $ do
            forM_ (debuggerFiles h) $ \file -> do
                shMkDir $ takeDirectory (tmp </> debuggerHtmlPath h </> file)
                shCommandToFileWith_ ioargs "debug-pp" [(debuggerPath h </> file)] (tmp </> debuggerHtmlPath h </> file)
            forM_ (debuggerInstrumentedFiles h) $ \file -> do
                shMkDir $ takeDirectory (tmp </> debuggerHtmlPath h </> file)
                shCp (debuggerInstrumentedPath h </> file) (tmp </> debuggerHtmlPath h </> file)
            
            let imports = concatMap (\modu -> "import " ++ modu ++ "\n") (debuggerModules h)
            let mainfile = imports ++ "\n"
                        ++ "import Debug.Hoed.Extras" ++ "\n"
                        ++ "main = do" ++ "\n"
                        ++ "  let prog = " ++ debuggerProgram h ++ "\n"
                        ++ "  let args = defaultHoedExtrasArgs { jshood = Deploy, jshoed = Deploy, debug = Deploy, datapath = Just (return " ++ show htmldatapath ++ ")}" ++ "\n"
                        ++ "  runHoedExtrasO args (print prog)" ++ "\n"
            
            shWriteFile' (tmp </> debuggerHtmlPath h </> "Main.hs") mainfile
            
        let dir = (tmp </> debuggerHtmlPath h)
        iores <- orIOResult $ runBaseSh $ do
            shCd dir
            shGhcWith ghcArgs ["Main.hs"]
        addMessageToError (pretty iores) $ runBaseSh $ do
            shCd dir
            exec <- shExec "Main"
            shCommandWith_ ioArgs exec extras
        
        let debugPath  = debuggerHtmlPath h </> "debug.html"
        let jshoodPath = debuggerHtmlPath h </> "jshood.html"
        let jshoedPath = debuggerHtmlPath h </> "jshoed.html"
        
        hakyllRules $ do
            -- copy the debugger data files
            let globdata = (fromGlob $ "debug" </> "img" </> "*.png")
                      .||. (fromGlob $ "debug" </> "web/JsHoed.jsexe" </> "*.js")
            match globdata $ do
                route   $ idRoute`composeRoutes` funRoute (hakyllRoute hp)
                compile $ copyFileCompiler
            
            -- copy the debugger generated documentation
            match (fromGlob $ tmp </> debuggerHtmlPath h </> "*.html") $ do
                route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                compile $ getResourceString >>= hakyllCompile hp
        return (hakyllRoute hp $ debugPath,hakyllRoute hp $ jshoodPath,hakyllRoute hp $ jshoedPath)

copyDebuggerFiles :: (MonadIO m,HaapStack t m) => Configuration -> Haap t m ()
copyDebuggerFiles cfg = do
    datapath <- runBaseIO' $ hoedExtrasDataPath
    xs <- runBaseIO' $ listDirectory datapath
    runBaseSh $ forM_ xs $ \x -> shCpRecursive (datapath </> x) (providerDirectory cfg </> "debug" </> x)    
    
    