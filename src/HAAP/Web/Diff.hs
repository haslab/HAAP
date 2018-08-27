{-
HAAP: Haskell Automated Assessment Platform

This module runs the @Diff@ plugin to show the difference between source and target files.

-}

{-# LANGUAGE EmptyDataDecls, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}

module HAAP.Web.Diff
    ( DiffArgs(..)
    , runDiff
    ) where

import HAAP.Compiler.GHC
import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils
import HAAP.Plugin
import HAAP.Shelly

import Data.Default
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as Text
import Data.Proxy

import Control.Monad.Reader as Reader

import System.FilePath

data DiffArgs = DiffArgs
    { diffTitle :: String
    , diffFiles :: [(String,String,String)] -- (file name,source content, target content)
    , diffHtmlPath :: FilePath -- relative to the project path
    }

runDiff :: (MonadIO m,HasPlugin Hakyll t m) => DiffArgs -> Haap t m FilePath
runDiff diff = do
    hp <- getHakyllP
    let homerrorpath = diffHtmlPath diff
    orErrorHakyllPage homerrorpath homerrorpath $ do
        tmp <- getProjectTmpPath
        hakyllRules $ do
            -- copy the generated documentation
            create [fromFilePath $ diffHtmlPath diff] $ do
                route $  idRoute `composeRoutes` funRoute (hakyllRoute hp)
                compile $ do
                    let fileCtx = field "diffFile" (return . fst3 . itemBody)
                               `mappend` field "diffSource" (return . snd3 . itemBody)
                               `mappend` field "diffTarget" (return . thr3 . itemBody)
                    let homCtx = constField "projectpath" (fileToRoot $ hakyllRoute hp $ diffHtmlPath diff)
                               `mappend` listField "diffFiles" fileCtx (mapM makeItem $ diffFiles diff)
                               `mappend` constField "pageTitle" (diffTitle diff)
                    makeItem "" >>= loadAndApplyHTMLTemplate "templates/diff.html" homCtx >>= hakyllCompile hp
        return (hakyllRoute hp $ diffHtmlPath diff)
      



  