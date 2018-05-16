{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Xterm@ plugin to compile _Xterm_ animations (<https://github.com/google/Xterm>) to HTML webpages.
-}


{-# LANGUAGE UndecidableInstances, FlexibleContexts, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, EmptyDataDecls, OverloadedStrings #-}

module HAAP.Web.Graphics.Xterm where

import HAAP.Core
import HAAP.IO
import HAAP.Compiler.GHCJS
import HAAP.Utils
import HAAP.Web.Hakyll 
import HAAP.Pretty
import HAAP.Plugin
import HAAP.Shelly

import Control.Monad
import Control.Monad.Reader as Reader
import Control.Monad.Except

import Data.Foldable
import Data.Either
import Data.String
import Data.Traversable
import Data.Default
import Data.Proxy
import qualified Data.Text as Text

import System.FilePath
import System.Directory
import System.Process

import qualified Shelly as Sh

--import Codec.Picture.Metadata
--import Codec.Picture

import XtermArgs

data Xterm

instance HaapPlugin Xterm where
    type PluginI Xterm = XtermArgs
    type PluginO Xterm = ()
    type PluginT Xterm = ReaderT XtermArgs
    type PluginK Xterm t m = (MonadIO m)
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . unComposeT) m
        return (x,())

data XtermArgs = XtermArgs
    { xtExecutable :: Either FilePath FilePath -- graphical web applications to compile with ghjs and Xterm libraries, or a link to an existing runmain.js file
    , xtTitle :: String
    , xtGHCJS :: GHCJSArgs
    , xtIO :: IOArgs
    , xtHtmlPath :: FilePath -- relative path to the project to store Xterm results
    }

useAndRunXterm :: (MonadIO m,HasPlugin Hakyll t m) => XtermArgs -> Haap t m FilePath
useAndRunXterm args = usePlugin_ (return args) $ runXterm

runXterm :: (MonadIO m,HasPlugin Hakyll t m,HasPlugin Xterm t m) => Haap t m FilePath
runXterm = do
    hp <- getHakyllP
    xt <- liftHaap $ liftPluginProxy (Proxy::Proxy Xterm) $ Reader.ask
    let xterrorpath = addExtension (xtHtmlPath xt) "html"
    orErrorHakyllPage xterrorpath xterrorpath $ do
        tmp <- getProjectTmpPath
        let (tpltfile,textmessage) = ("templates/xterm.html","")
        -- compile files with ghcjs
        let ghcjs = xtGHCJS xt
        let io = xtIO xt
        (destdir,destfolder) <- case xtExecutable xt of
            Left xtexec -> do
                let exec = takeFileName xtexec
                let destdir = dropExtension (xtHtmlPath xt </> exec)
                let destfolder = addExtension destdir "jsexe"
                return (destdir,destfolder)
            Right htmldir -> return (xtHtmlPath xt,xtHtmlPath xt)
        
        res <- case xtExecutable xt of
            Left xtexec -> orIOResult $ runBaseShWith (io) $ do
                let (dir,exec) = splitFileName xtexec
                let ghcjs' = ghcjs { ghcjsArgs = ghcjsArgs ghcjs ++ ["-o",dirToRoot dir </> tmp </> destdir], ghcjsIO = io }
                Sh.mkdir_p (fromString $ tmp </> destfolder)
                shCd dir
                --Sh.setenv "GHC_PACKAGE_PATH" (Text.pack $ concatPaths ghcpackagedbs)
                --Sh.setenv "GHCJS_PACKAGE_PATH" (Text.pack $ concatPaths ghcjspackagedbs)
                shGhcjsWith ghcjs' [exec]
            otherwise -> do
                let precompiled = Text.pack $ "Pre-compiled at " ++ show (xtExecutable xt)
                return $ IOResult 0 precompiled precompiled
        
        if resOk res
            then addMessageToError (pretty res) $ do
                
                getHakyllArgs >>= copyXtermDataFiles . hakyllCfg
                
                hakyllRules $ do 
                    let message = show $ text "=== Compiling ===" $+$ doc res $+$ "=== Running ==="
                    match (fromGlob $ tmp </> destfolder </> "*.html") $ do
                        route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                        compile $ getResourceString >>= hakyllCompile hp
                    let auxFiles = fromGlob (tmp </> destfolder </> "*.js")
                                   .||. fromGlob (tmp </> destfolder </> "*.externs")
                                   .||. fromGlob (tmp </> destfolder </> "*.webapp")
                                   .||. fromGlob (tmp </> destfolder </> "*.stats")
                    when (isLeft $ xtExecutable xt) $ match auxFiles $ do
                        route   $ relativeRoute tmp
                        compile copyFileCompiler
                    let runpath = case xtExecutable xt of
                                    Left _ -> "."
                                    Right html -> dirToRoot destfolder </> html
                    create [fromFilePath $ destfolder </> "run.html"] $ do
                        route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
                        compile $ do
                            let xtCtx = constField "title" (xtTitle xt)
                                      `mappend` constField "projectpath" (dirToRoot destfolder)
                                      `mappend` constField "runpath" runpath
                                      `mappend` constField "message" message
                                      `mappend` constField "textmessage" textmessage
                            makeItem "" >>= loadAndApplyHTMLTemplate tpltfile xtCtx >>= hakyllCompile hp
                    matchXtermFiles
                return (hakyllRoute hp $ destfolder </> "run.html")
            else throwError $ HaapException $ pretty res

instance HaapMonad m => HasPlugin Xterm (ReaderT XtermArgs) m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT (ReaderT XtermArgs) m (t2 m)) => HasPlugin Xterm (ComposeT (ReaderT XtermArgs) t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m

copyXtermDataFiles :: (MonadIO m,HaapStack t m) => Configuration -> Haap t m ()
copyXtermDataFiles cfg = do
    datapath <- runBaseIO' $ getXtermFile ""
    xs <- runBaseIO' $ listDirectory datapath
    runBaseSh $ forM_ xs $ \x -> shCpRecursive (datapath </> x) (providerDirectory cfg </> x)

matchXtermFiles :: Rules ()
matchXtermFiles = do
    match (fromGlob ("js" </> "*.js")) $ do
        route idRoute
        compile copyFileCompiler
    match (fromGlob ("css" </> "*.css")) $ do
        route idRoute
        compile compressCssCompiler






