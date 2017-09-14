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

import System.FilePath
import System.Directory

import qualified Shelly as Sh

data CodeWorldArgs args = CodeWorldArgs
    { cwExecutables :: [FilePath] -- graphical web applications to compile with ghjs and codeworld libraries
    , cwGHCJS :: args -> GHCJSArgs
    , cwIO :: args -> IOArgs
    , cwHtmlPath :: FilePath -- relative path to the project to store codeworld results
    , cwGHJSPackages :: [FilePath] -- additional ghcjs library folders
    }

runCodeWorld :: CodeWorldArgs args -> Haap p args db (Rules (),[FilePath])
runCodeWorld cw = do
    htmls <- forM (cwExecutables cw) $ \path -> do
        -- compile files with ghcjs
        ghcjs <- Reader.reader (cwGHCJS cw)
        io <- Reader.reader (cwIO cw)
        let (dir,exec) = splitFileName path
        let destdir = dropExtension (cwHtmlPath cw </> exec)
        let destfolder = addExtension destdir "jsexe"
        let packages = map (\x -> "-package-db=" ++ toRoot dir </> x) (cwGHJSPackages cw)
        let ghcjs' = ghcjs { ghcjsArgs = packages ++ ghcjsArgs ghcjs ++ ["-o",toRoot dir </> destdir] }
        res <- runSh $ do
            Sh.mkdir_p (fromString destfolder)
            shCd dir
            shGhcjsWith io ghcjs' [exec]
        return (res,destfolder)
    let rules = do
        forM_ htmls $ \(res,html) -> do
            let message = show $ text "=== Compiling ===" $+$ doc res $+$ "=== Running ==="
            match (fromGlob $ html </> "*") $ do
                route   idRoute
                compile copyFileCompiler
            create [fromFilePath $ html </> "run.html"] $ do
                route idRoute
                compile $ do
                    let cwCtx = constField "title" html
                              `mappend` constField "projectpath" (toRoot $ html </> "run.html")
                              `mappend` constField "message" message
                    makeItem "" >>= loadAndApplyTemplate "templates/codeworld.html" cwCtx
    
    let runs = map ((</> "run.html") . snd) htmls
    return (rules,runs)
    
