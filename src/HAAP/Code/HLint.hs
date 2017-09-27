{-# LANGUAGE OverloadedStrings #-}

module HAAP.Code.HLint where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils

import Data.Default
import qualified Data.Text as Text

import qualified Control.Monad.Reader as Reader

import System.FilePath

data HLintArgs = HLintArgs
    { hlintSandbox :: Maybe FilePath
    , hlintArgs :: [String]
    , hlintPath :: FilePath -- path relative to the project where to execute the hlint command
    , hlintFiles :: [FilePath] -- relative to the path where hlint is executed
    , hlintHtmlPath :: FilePath -- relative to the project path
    }

runHLint :: HakyllP -> HLintArgs -> Haap p args db Hakyll FilePath
runHLint hp h = do
    tmp <- getProjectTmpPath
    let ioArgs = def { ioSandbox = fmap (dirToRoot (hlintPath h) </>) (hlintSandbox h) }
    let extras = hlintArgs h
    let files = hlintFiles h
    let html = dirToRoot (hlintPath h) </> tmp </> hlintHtmlPath h
    orErrorWritePage (tmp </> hlintHtmlPath h) mempty $ runSh $ do
        shCd $ hlintPath h
        shCommandWith ioArgs "hlint" (extras++["--report="++html]++files)
--    runIO $ putStrLn $ show $ resStderr res
--    runIO $ putStrLn $ show $ resStdout res
    hakyllRules $ do
        -- copy the hlint generated documentation
        match (fromGlob $ tmp </> hlintHtmlPath h) $ do
            route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
            compile $ getResourceString >>= hakyllCompile hp
    return (hakyllRoute hp $ hlintHtmlPath h)
      