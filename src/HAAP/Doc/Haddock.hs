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
    { haddockSandbox :: Bool
    , haddockTitle :: String
    , haddockArgs :: [String]
    , haddockPath :: FilePath -- path relative to the project where to execute the haddock command
    , haddockFiles :: [FilePath] -- relative to the path where haddock is executed
    , haddockHtmlPath :: FilePath -- relative to the project path
    }

runHaddock :: HaddockArgs -> Haap p args db (Rules ())
runHaddock h = do
    let ioArgs = def { ioSandbox = haddockSandbox h }
    let extras = haddockArgs h
    let files = haddockFiles h
    let html = toRoot (haddockHtmlPath h) </> haddockHtmlPath h
    res <- runSh $ do
        shCd $ haddockPath h
        shCommandWith ioArgs "haddock" (extras++["-h","-o",html]++files)
--    runIO $ putStrLn $ show $ resStderr res
--    runIO $ putStrLn $ show $ resStdout res
    return $ do
        -- copy the haddock generated documentation
        match (fromGlob $ (haddockHtmlPath h) </> "*") $ do
            route   idRoute
            compile copyFileCompiler
        -- generate a documentation page with the haddock report and a link to the documentation
        create [fromFilePath $ addExtension (haddockHtmlPath h) "html"] $ do
            route idRoute
            compile $ do
                let docCtx = constField "title" (haddockTitle h)
                           `mappend` constField "projectpath" (toRoot $ haddockPath h)
                           `mappend` constField "stdout" (Text.unpack $ resStdout res)
                           `mappend` constField "stderr" (Text.unpack $ resStderr res)
                           `mappend` constField "link" (haddockHtmlPath h </> "index.html")
                makeItem "" >>= loadAndApplyTemplate "templates/doc.html" docCtx
        