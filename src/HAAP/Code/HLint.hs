{-
HAAP: Haskell Automated Assessment Platform

This module provides the @HLint@ plugin that invokes the external _hlint_ tool (<https://hackage.haskell.org/package/hlint>) to give automatic suggestions on how to improve overall code quality.

-}

{-# LANGUAGE EmptyDataDecls, OverloadedStrings, TypeOperators, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}

module HAAP.Code.HLint where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils
import HAAP.Plugin
import HAAP.Shelly

import Data.Default
import qualified Data.Text as Text
import Data.Proxy

import Control.Monad.Reader as Reader

import System.FilePath

data HLint 

instance HaapPlugin HLint where
    type PluginI HLint = HLintArgs
    type PluginO HLint = ()
    type PluginT HLint = ReaderT HLintArgs
    type PluginK HLint t m = ()
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . getComposeT) m
        return (x,())

instance HaapMonad m => HasPlugin HLint (ReaderT HLintArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin HLint (ComposeT (ReaderT HLintArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m

data HLintArgs = HLintArgs
    { hlintSandbox :: Sandbox
    , hlintArgs :: [String]
    , hlintPath :: FilePath -- path relative to the project where to execute the hlint command
    , hlintFiles :: [FilePath] -- relative to the path where hlint is executed
    , hlintHtmlPath :: FilePath -- relative to the project path
    }

useAndRunHLint :: (MonadIO m,HasPlugin Hakyll t m) => HLintArgs -> Haap t m FilePath
useAndRunHLint args = usePlugin_ (return args) $ runHLint

runHLint :: (MonadIO m,HasPlugin Hakyll t m,HasPlugin HLint t m) => Haap t m FilePath
runHLint = do
    h <- liftHaap $ liftPluginProxy (Proxy::Proxy HLint) $ Reader.ask
    hp <- getHakyllP
    let hlinterrorpath = addExtension (hlintHtmlPath h) "html"
    orErrorHakyllPage hlinterrorpath hlinterrorpath $ do
        tmp <- getProjectTmpPath
        let ioArgs = def { ioSandbox = mapSandboxCfg (dirToRoot (hlintPath h) </>) (hlintSandbox h) }
        let extras = hlintArgs h
        let files = hlintFiles h
        let html = dirToRoot (hlintPath h) </> tmp </> hlintHtmlPath h
        orErrorWritePage (tmp </> hlintHtmlPath h) mempty $ runBaseSh $ do
            shCd $ hlintPath h
            shCommandWith ioArgs "hlint" (extras++["--report="++html]++files)
--        runIO $ putStrLn $ show $ resStderr res
--        runIO $ putStrLn $ show $ resStdout res
        hakyllFocus [tmp </> hlintHtmlPath h] $ hakyllRules $ do
            -- copy the hlint generated documentation
            match (fromGlob $ tmp </> hlintHtmlPath h) $ do
                route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                compile $ getResourceString >>= hakyllCompile hp
        return (hakyllRoute hp $ hlintHtmlPath h)

      