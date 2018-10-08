{-
HAAP: Haskell Automated Assessment Platform

This module provides the @CodeWorld@ plugin to compile _CodeWorld_ animations (<https://github.com/google/codeworld>) to HTML webpages.
-}


{-# LANGUAGE UndecidableInstances, TypeOperators, FlexibleContexts, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, EmptyDataDecls, OverloadedStrings #-}

module HAAP.Web.Graphics.CodeWorld where

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
--import Control.Monad.Except
import Control.Exception.Safe
import Control.Monad.Morph

import Data.Foldable
import Data.Either
import Data.String
import Data.Traversable
import Data.Default
import Data.Proxy
import qualified Data.Text as T

import System.FilePath
import System.Directory
import System.Process

import qualified Shelly as Sh

--import Codec.Picture.Metadata
--import Codec.Picture

data CodeWorld

instance HaapPlugin CodeWorld where
    type PluginI CodeWorld = CodeWorldArgs
    type PluginO CodeWorld = ()
    type PluginT CodeWorld = ReaderT CodeWorldArgs
    type PluginK CodeWorld t m = (MonadIO m)
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . getComposeT) m
        return (x,())

data CWTemplate
    = CWGame CWGameType
    | CWDraw CWDrawType T.Text
    
data CWGameType 
    = CWGameFullscreen
    | CWGameConsole
    
data CWDrawType 
    = CWDrawFixed
    | CWDrawButton
    | CWDrawFullscreen

data CodeWorldArgs = CodeWorldArgs
    { cwExecutable :: Either FilePath FilePath -- graphical web applications to compile with ghcjs and codeworld libraries, or a link to an existing runmain.js file
    , cwTitle :: T.Text
    , cwTemplate :: CWTemplate 
    , cwGHCJS :: GHCJSArgs
    , cwIO :: IOArgs
    , cwHtmlPath :: FilePath -- relative path to the project to store codeworld results
    , cwImages :: [(String,FilePath)] -- a list of html identifiers and respective local files for loading images
    , cwAudios :: [(String,FilePath)]
    }

useAndRunCodeWorld :: (MonadIO m,HasPlugin Hakyll t m) => CodeWorldArgs -> Haap t m FilePath
useAndRunCodeWorld args = usePlugin_ (return args) $ runCodeWorld

runCodeWorld :: (MonadIO m,HasPlugin Hakyll t m,HasPlugin CodeWorld t m) => Haap t m FilePath
runCodeWorld = do
    hp <- getHakyllP
    cw <- liftHaap $ liftPluginProxy (Proxy::Proxy CodeWorld) $ Reader.ask
    let cwerrorpath = addExtension (cwHtmlPath cw) "html"
    orErrorHakyllPage cwerrorpath cwerrorpath $ do
        tmp <- getProjectTmpPath
        let (tpltfile,textmessage) = case cwTemplate cw of
                                        CWGame CWGameConsole -> ("templates/cw-game-console.html","")
                                        CWGame CWGameFullscreen -> ("templates/cw-game-fullscreen.html","")
                                        CWDraw CWDrawFixed msg -> ("templates/cw-draw-fixed.html",msg)
                                        CWDraw CWDrawButton msg -> ("templates/cw-draw-button.html",msg)
                                        CWDraw CWDrawFullscreen msg -> ("templates/cw-draw-fullscreen.html",msg)
        -- compile files with ghcjs
        let ghcjs = cwGHCJS cw
        let io = cwIO cw
        (destdir,destfolder) <- case cwExecutable cw of
            Left cwexec -> do
                let exec = takeFileName cwexec
                let destdir = dropExtension (cwHtmlPath cw </> exec)
                let destfolder = addExtension destdir "jsexe"
                return (destdir,destfolder)
            Right htmldir -> return (cwHtmlPath cw,cwHtmlPath cw)
        
        res <- case cwExecutable cw of
            Left cwexec -> orIOResult $ runBaseShWith (io) $ do
                let (dir,exec) = splitFileName cwexec
                let ghcjs' = ghcjs { ghcjsMake = True, ghcjsArgs = ghcjsArgs ghcjs ++ ["-o",dirToRoot dir </> tmp </> destdir], ghcjsIO = io }
                Sh.mkdir_p (fromString $ tmp </> destfolder)
                shCd dir
                --Sh.setenv "GHC_PACKAGE_PATH" (T.pack $ concatPaths ghcpackagedbs)
                --Sh.setenv "GHCJS_PACKAGE_PATH" (T.pack $ concatPaths ghcjspackagedbs)
                shGhcjsWith ghcjs' [exec]
            otherwise -> do
                let precompiled = "Pre-compiled at " <> prettyText (cwExecutable cw)
                return $ IOResult 0 precompiled precompiled
        let images = (cwImages cw)
        let audios = (cwAudios cw)
        
        if resOk res
            then addMessageToError (prettyText res) $ do
                hakyllFocus ["templates",tmp </> destfolder,destfolder] $ hakyllRules $ do 
                    let message = text "=== Compiling ===" $+$ pretty res $+$ text "=== Running ==="
                    match (fromGlob $ tmp </> destfolder </> "*.html") $ do
                        route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                        compile $ getResourceString >>= hakyllCompile hp
                    let auxFiles = fromGlob (tmp </> destfolder </> "*.js")
                                   .||. fromGlob (tmp </> destfolder </> "*.externs")
                                   .||. fromGlob (tmp </> destfolder </> "*.webapp")
                                   .||. fromGlob (tmp </> destfolder </> "*.stats")
                    when (isLeft $ cwExecutable cw) $ match auxFiles $ do
                        route $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                        compile copyFileCompiler
                    let runpath = case cwExecutable cw of
                                    Left _ -> "."
                                    Right html -> dirToRoot destfolder </> html
                    create [fromFilePath $ destfolder </> "run.html"] $ do
                        route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
                        compile $ do
                            let mkImg s = s
                            let imgCtx = field "imgid" (return . fst . itemBody)
                                       `mappend` constField "projectpath" (dirToRoot destfolder)
                                       `mappend` field "imgfile" (return . mkImg . snd . itemBody)
                                       `mappend` constField "runpath" (runpath)
                            let audioCtx = field "audioid" (return . fst . itemBody)
                                       `mappend` constField "projectpath" (dirToRoot destfolder)
                                       `mappend` field "audiofile" (return . mkImg . snd . itemBody)
                                       `mappend` field "audiotype" (return . filter (/='.') . takeExtension . snd . itemBody)
                                       `mappend` constField "runpath" (runpath)
                            let cwCtx = constField "title" (T.unpack $ cwTitle cw)
                                      `mappend` constField "projectpath" (dirToRoot destfolder)
                                      `mappend` constField "runpath" runpath
                                      `mappend` constField "message" (renderDocString message)
                                      `mappend` constField "textmessage" (T.unpack textmessage)
                                      `mappend` listField "images" imgCtx (mapM makeItem images)
                                      `mappend` listField "audios" audioCtx (mapM makeItem audios)
                            makeItem "" >>= loadAndApplyHTMLTemplate tpltfile cwCtx >>= hakyllCompile hp
                    
                return (hakyllRoute hp $ destfolder </> "run.html")
            else throw $ HaapException $ prettyText res

instance HaapMonad m => HasPlugin CodeWorld (ReaderT CodeWorldArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin CodeWorld (ComposeT (ReaderT CodeWorldArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m

--loadCodeWorldImages :: (MonadIO m,HaapStack t m) => [(String,FilePath)] -> Haap t m [(String,FilePath),(Int,Int)]
--loadCodeWorldImages xs = forM xs $ \(n,fp) -> do
--    e <- runBaseIO $ readImageWithMetadata fp
--    case e of
--        Left err -> throwError $ HaapException $ "failed loading codeworld image" ++ show fp ++ "\n" ++ pretty err
--        Right (_,md) ->
--            mbw <- Juicy.lookup Width md
--            w <- case mbw of
--                Nothing -> throwError $ HaapException $ "failed loading codeworld image width" ++ show fp ++ "\n" ++ pretty err
--                Just w -> return w
--            mbh <- Juicy.lookup Height md
--            h <- case mbh of
--                Nothing -> throwError $ HaapException $ "failed loading codeworld image height" ++ show fp ++ "\n" ++ pretty err
--                Just h -> return h
--            return (w,h)
--    return ((n,fp),(w,h))




