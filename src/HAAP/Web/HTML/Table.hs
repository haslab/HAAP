{-
HAAP: Haskell Automated Assessment Platform

This module provides functions for rendering HTML tables.
-}

{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module HAAP.Web.HTML.Table where

import HAAP.Core
import HAAP.Pretty
import HAAP.Test.Spec
import HAAP.Test.Rank
import HAAP.Utils
import HAAP.Web.Hakyll
import HAAP.Plugin

import Data.Traversable
import Data.List

import qualified Control.Monad.Reader as Reader

data Url a = Url FilePath a
  deriving (Eq,Ord)

instance Show a => Show (Url a) where
    show (Url url x) = "<a href=" ++ show url ++ ">" ++ show x ++ "</a>"

instance Out a => Out (Url a) where
    docPrec i x = doc x
    doc (Url url x) = text "<a href=" <> text (show url) <> text ">" <> doc x <> text "</a>"

data HaapTable = HaapTable
    { haapTableTitle :: String
    , haapTableHeaders :: [String]
    , haapTableRows :: [[String]]
    , haapTablePath :: FilePath
    }

renderHaapTable :: HasPlugin Hakyll t m => HaapTable -> Haap t m FilePath
renderHaapTable t = do
    hp <- getHakyllP
    hakyllRules $ create [fromFilePath $ haapTablePath t] $ do
        route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
        compile $ do
            let headerCtx = field "header" (return . itemBody)
            let colCtx = field "col" (return . itemBody)
                       `mappend` constField "class" ""
            let rowCtx = listFieldWith "cols" colCtx (mapM makeItem . itemBody)
            let pageCtx = constField "title" (haapTableTitle t)
                        `mappend` constField "projectpath" (fileToRoot $ hakyllRoute hp $ haapTablePath t)
                        `mappend` listField "headers" headerCtx (mapM makeItem $ haapTableHeaders t)
                        `mappend` listField "rows" rowCtx (mapM makeItem $ haapTableRows t)
            makeItem "" >>= loadAndApplyHTMLTemplate "templates/table.html" pageCtx >>= hakyllCompile hp
    return (hakyllRoute hp $ haapTablePath t)




