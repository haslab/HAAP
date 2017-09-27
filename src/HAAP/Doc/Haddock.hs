{-# LANGUAGE OverloadedStrings #-}

module HAAP.Doc.Haddock where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils

import Data.Default
import qualified Data.Text as Text

import qualified Control.Monad.Reader as Reader

import System.FilePath

data HaddockArgs = HaddockArgs
    { haddockSandbox :: Maybe FilePath
    , haddockTitle :: String
    , haddockArgs :: [String]
    , haddockPath :: FilePath -- path relative to the project where to execute the haddock command
    , haddockFiles :: [FilePath] -- relative to the path where haddock is executed
    , haddockHtmlPath :: FilePath -- relative to the project path
    }

runHaddock :: HakyllP -> HaddockArgs -> Haap p args db Hakyll FilePath
runHaddock hp h = do
    tmp <- getProjectTmpPath
    let ioArgs = def { ioSandbox = fmap (dirToRoot (haddockPath h) </>) (haddockSandbox h) }
    let extras = haddockArgs h
    let files = haddockFiles h
    let html = dirToRoot (haddockPath h) </> tmp </> haddockHtmlPath h
    let indexhtml = addExtension (haddockHtmlPath h) "html"
    res <- orErrorWritePage (tmp </> indexhtml) mempty $ runSh $ do
        shCd $ haddockPath h
        shCommandWith ioArgs "haddock" (extras++["-h","-o",html]++files)
--    runIO $ putStrLn $ show $ resStderr res
--    runIO $ putStrLn $ show $ resStdout res
    hakyllRules $ do
        -- copy the haddock generated documentation
        match (fromGlob $ (tmp </> haddockHtmlPath h) </> "*.html") $ do
            route   $ relativeRoute tmp `composeRoutes` hakyllRoute hp
            compile $ getResourceString >>= hakyllCompile hp
        let auxFiles = fromGlob (tmp </> haddockHtmlPath h </> "*.js")
                       .||. fromGlob (tmp </> haddockHtmlPath h </> "*.png")
                       .||. fromGlob (tmp </> haddockHtmlPath h </> "*.gif")
                       .||. fromGlob (tmp </> haddockHtmlPath h </> "*.css")
        match auxFiles $ do
            route   $ relativeRoute tmp
            compile $ copyFileCompiler
        -- generate a documentation page with the haddock report and a link to the documentation
        create [fromFilePath indexhtml] $ do
            route $ idRoute `composeRoutes` hakyllRoute hp
            compile $ do
                let docCtx = constField "title" (haddockTitle h)
                           `mappend` constField "projectpath" (dirToRoot $ haddockPath h)
                           `mappend` constField "stdout" (Text.unpack $ resStdout res)
                           `mappend` constField "stderr" (Text.unpack $ resStderr res)
                           `mappend` constField "link" (takeFileName (haddockHtmlPath h) </> "index.html")
                makeItem "" >>= loadAndApplyHTMLTemplate "templates/doc.html" docCtx >>= hakyllCompile hp
    return indexhtml
        