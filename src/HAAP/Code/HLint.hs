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
    { hlintSandbox :: Bool
    , hlintArgs :: [String]
    , hlintPath :: FilePath -- path relative to the project where to execute the hlint command
    , hlintFiles :: [FilePath] -- relative to the path where hlint is executed
    , hlintHtmlPath :: FilePath -- relative to the project path
    }

runHLint :: HLintArgs -> Haap p args db (Rules (),FilePath)
runHLint h = do
    let ioArgs = def { ioSandbox = hlintSandbox h }
    let extras = hlintArgs h
    let files = hlintFiles h
    let html = toRoot (hlintHtmlPath h) </> hlintHtmlPath h
    res <- runSh $ do
        shCd $ hlintPath h
        shCommandWith ioArgs "hlint" (extras++["--report="++html]++files)
--    runIO $ putStrLn $ show $ resStderr res
--    runIO $ putStrLn $ show $ resStdout res
    let rules = do
        -- copy the hlint generated documentation
        match (fromGlob $ hlintHtmlPath h) $ do
            route   idRoute
            compile copyFileCompiler
    return (rules,hlintHtmlPath h)
      
        