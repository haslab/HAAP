{-# LANGUAGE OverloadedStrings #-}

module HAAP.Web.Graphics.CodeWorld where

import HAAP.Core
import HAAP.IO
import HAAP.Compiler.GHCJS
import HAAP.Utils
import HAAP.Web.Hakyll 
import HAAP.Pretty

import qualified Control.Monad.Reader as Reader

import Data.Foldable
import Data.String
import Data.Traversable
import Data.Default
import qualified Data.Text as Text

import System.FilePath
import System.Directory
import System.Process

import qualified Shelly as Sh

data CWTemplate = CWGame | CWDraw String

data CodeWorldArgs args = CodeWorldArgs
    { cwExecutable :: FilePath -- graphical web applications to compile with ghjs and codeworld libraries
    , cwTitle :: String
    , cwTemplate :: CWTemplate 
    , cwGHCJS :: args -> GHCJSArgs
    , cwIO :: args -> IOArgs
    , cwHtmlPath :: FilePath -- relative path to the project to store codeworld results
    , cwImages :: [(String,FilePath)] -- a list of html identifiers and respective local files for loading images
    }

runCodeWorld :: HakyllP -> CodeWorldArgs args -> Haap p args db Hakyll FilePath
runCodeWorld hp cw = do
    tmp <- getProjectTmpPath
    let (tpltfile,textmessage) = case cwTemplate cw of
                                    CWGame -> ("templates/cw-game.html","")
                                    CWDraw msg -> ("templates/cw-draw.html",msg)
    -- compile files with ghcjs
    ghcjs <- Reader.reader (cwGHCJS cw)
    io <- Reader.reader (cwIO cw)
    let (dir,exec) = splitFileName (cwExecutable cw)
    let destdir = dropExtension (cwHtmlPath cw </> exec)
    let destfolder = addExtension destdir "jsexe"
    let ghcjs' = ghcjs { ghcjsArgs = ghcjsArgs ghcjs ++ ["-o",dirToRoot dir </> tmp </> destdir] }
    
    res <- runShWith (const io) $ do
        Sh.mkdir_p (fromString $ tmp </> destfolder)
        shCd dir
        --Sh.setenv "GHC_PACKAGE_PATH" (Text.pack $ concatPaths ghcpackagedbs)
        --Sh.setenv "GHCJS_PACKAGE_PATH" (Text.pack $ concatPaths ghcjspackagedbs)
        shGhcjsWith io ghcjs' [exec]
--    runIO $ system "ghcjs --user"
--    res <- runIO $ ioGhcjsWith io' ghcjs' [cwExecutable cw]
    let images = (cwImages cw)
        
    hakyllRules $ do
        
        let message = show $ text "=== Compiling ===" $+$ doc res $+$ "=== Running ==="
        match (fromGlob $ tmp </> destfolder </> "*.html") $ do
            route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
            compile $ getResourceString >>= hakyllCompile hp
        let auxFiles = fromGlob (tmp </> destfolder </> "*.js")
                       .||. fromGlob (tmp </> destfolder </> "*.externs")
                       .||. fromGlob (tmp </> destfolder </> "*.webapp")
                       .||. fromGlob (tmp </> destfolder </> "*.stats")
        match auxFiles $ do
            route   $ relativeRoute tmp
            compile copyFileCompiler
        create [fromFilePath $ destfolder </> "run.html"] $ do
            route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
            compile $ do
                let mkImg s = s
                let imgCtx = field "imgid" (return . fst . itemBody)
                           `mappend` constField "projectpath" (dirToRoot destfolder)
                           `mappend` field "imgfile" (return . mkImg . snd . itemBody)
                let cwCtx = constField "title" (cwTitle cw)
                          `mappend` constField "projectpath" (dirToRoot destfolder)
                          `mappend` constField "message" message
                          `mappend` constField "textmessage" textmessage
                          `mappend` listField "images" imgCtx (mapM makeItem images)
                makeItem "" >>= loadAndApplyHTMLTemplate tpltfile cwCtx >>= hakyllCompile hp
        
    return (hakyllRoute hp $ destfolder </> "run.html")
    
