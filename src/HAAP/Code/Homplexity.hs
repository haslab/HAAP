{-# LANGUAGE OverloadedStrings #-}

module HAAP.Code.Homplexity where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils

import Data.Default
import Data.List
import qualified Data.Text as Text

import qualified Control.Monad.Reader as Reader

import System.FilePath

data HomplexityArgs = HomplexityArgs
    { homplexitySandbox :: Maybe FilePath
    , homplexityArgs :: [String]
    , homplexityPath :: FilePath -- path relative to the project where to execute the homplexity command
    , homplexityFiles :: [FilePath] -- relative to the path where homplexity is executed
    , homplexityHtmlPath :: FilePath -- relative to the project path
    }

runHomplexity :: HakyllP -> HomplexityArgs -> Haap p args db Hakyll FilePath
runHomplexity hp h = do
    tmp <- getProjectTmpPath
    let ioArgs = def { ioSandbox = fmap (dirToRoot (homplexityPath h) </>) (homplexitySandbox h) }
    let extras = homplexityArgs h
    let files = homplexityFiles h
--    let html = dirToRoot (homplexityPath h) </> tmp </> homplexityHtmlPath h
    res <- orErrorWritePage (tmp </> homplexityHtmlPath h) mempty $ runSh $ do
        shCd $ homplexityPath h
        shCommandWith ioArgs "homplexity" (extras++files)
--    runIO $ putStrLn $ show $ resStderr res
--    runIO $ putStrLn $ show $ resStdout res
    let messages = parseMessages $ lines (Text.unpack $ resStdout res) ++ lines (Text.unpack $ resStderr res)
    hakyllRules $ do
        -- copy the homplexity generated documentation
        create [fromFilePath $ homplexityHtmlPath h] $ do
            route $  idRoute `composeRoutes` hakyllRoute hp
            compile $ do
                let msgCtx = field "class" (return . fst3 . itemBody)
                           `mappend` field "suggestion" (return . snd3 . itemBody)
                           `mappend` field "message" (return . thr3 . itemBody)
                let homCtx = constField "projectpath" (dirToRoot $ homplexityPath h)
                           `mappend` listField "messages" msgCtx (mapM makeItem messages)
                makeItem "" >>= loadAndApplyHTMLTemplate "templates/homplexity.html" homCtx >>= hakyllCompile hp
    return (homplexityHtmlPath h)
      
parseMessages [] = []
parseMessages (x:xs)
    | isPrefixOf "Warning:" x = ("hspec-warning","Warning",drop 8 x) : parseMessages xs
    | isPrefixOf "Debug:" x = ("hspec-debug","Debug",drop 6 x) : parseMessages xs
    | isPrefixOf "Critical:" x = ("hspec-failure","Critical",drop 9 x) : parseMessages xs
    | isPrefixOf "Info:" x = ("hspec-success","Info",drop 5 x) : parseMessages xs
    | otherwise = parseMessages xs