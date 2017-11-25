{-# LANGUAGE OverloadedStrings #-}

module HAAP.Web.Graphics.CodeWorld where

import HAAP.Core
import HAAP.IO
import HAAP.Compiler.GHCJS
import HAAP.Utils
import HAAP.Web.Hakyll 
import HAAP.Pretty

import Control.Monad
import qualified Control.Monad.Reader as Reader

import Data.Foldable
import Data.Either
import Data.String
import Data.Traversable
import Data.Default
import qualified Data.Text as Text

import System.FilePath
import System.Directory
import System.Process

import qualified Shelly as Sh

data CWTemplate = CWGame | CWDraw Bool String

data CodeWorldArgs args = CodeWorldArgs
    { cwExecutable :: Either FilePath FilePath -- graphical web applications to compile with ghjs and codeworld libraries, or a link to an existing runmain.js file
    , cwTitle :: String
    , cwTemplate :: CWTemplate 
    , cwGHCJS :: args -> GHCJSArgs
    , cwIO :: args -> IOArgs
    , cwHtmlPath :: FilePath -- relative path to the project to store codeworld results
    , cwImages :: [(String,FilePath)] -- a list of html identifiers and respective local files for loading images
    }

runCodeWorld :: HakyllP -> CodeWorldArgs args -> Haap p args db Hakyll FilePath
runCodeWorld hp cw = orErrorHakyllPage hp cwerrorpath cwerrorpath $ do
    tmp <- getProjectTmpPath
    let (tpltfile,textmessage) = case cwTemplate cw of
                                    CWGame -> ("templates/cw-game.html","")
                                    CWDraw False msg -> ("templates/cw-draw-fixed.html",msg)
                                    CWDraw True msg -> ("templates/cw-draw-button.html",msg)
    -- compile files with ghcjs
    ghcjs <- Reader.reader (cwGHCJS cw)
    io <- Reader.reader (cwIO cw)
    (destdir,destfolder) <- case cwExecutable cw of
        Left cwexec -> do
            let exec = takeFileName cwexec
            let destdir = dropExtension (cwHtmlPath cw </> exec)
            let destfolder = addExtension destdir "jsexe"
            return (destdir,destfolder)
        Right htmldir -> return (cwHtmlPath cw,cwHtmlPath cw)
    
    res <- case cwExecutable cw of
        Left cwexec -> runShIOResultWith (const io) $ do
            let (dir,exec) = splitFileName cwexec
            let ghcjs' = ghcjs { ghcjsArgs = ghcjsArgs ghcjs ++ ["-o",dirToRoot dir </> tmp </> destdir] }
            Sh.mkdir_p (fromString $ tmp </> destfolder)
            shCd dir
            --Sh.setenv "GHC_PACKAGE_PATH" (Text.pack $ concatPaths ghcpackagedbs)
            --Sh.setenv "GHCJS_PACKAGE_PATH" (Text.pack $ concatPaths ghcjspackagedbs)
            shGhcjsWith io ghcjs' [exec]
        otherwise -> do
            let precompiled = Text.pack $ "Pre-compiled at " ++ show (cwExecutable cw)
            return $ IOResult 0 precompiled precompiled
    let images = (cwImages cw)
        
    addMessageToError (pretty res) $ hakyllRules $ do
        
        let message = show $ text "=== Compiling ===" $+$ doc res $+$ "=== Running ==="
        match (fromGlob $ tmp </> destfolder </> "*.html") $ do
            route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
            compile $ getResourceString >>= hakyllCompile hp
        let auxFiles = fromGlob (tmp </> destfolder </> "*.js")
                       .||. fromGlob (tmp </> destfolder </> "*.externs")
                       .||. fromGlob (tmp </> destfolder </> "*.webapp")
                       .||. fromGlob (tmp </> destfolder </> "*.stats")
        when (isLeft $ cwExecutable cw) $ match auxFiles $ do
            route   $ relativeRoute tmp
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
                let cwCtx = constField "title" (cwTitle cw)
                          `mappend` constField "projectpath" (dirToRoot destfolder)
                          `mappend` constField "runpath" runpath
                          `mappend` constField "message" message
                          `mappend` constField "textmessage" textmessage
                          `mappend` listField "images" imgCtx (mapM makeItem images)
                makeItem "" >>= loadAndApplyHTMLTemplate tpltfile cwCtx >>= hakyllCompile hp
        
    return (hakyllRoute hp $ destfolder </> "run.html")
  where
    cwerrorpath = addExtension (cwHtmlPath cw) "html"
    
