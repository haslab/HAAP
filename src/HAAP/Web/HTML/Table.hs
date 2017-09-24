{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module HAAP.Web.HTML.Table where

import HAAP.Core
import HAAP.Pretty
import HAAP.Test.Spec
import HAAP.Test.Rank
import HAAP.Utils
import HAAP.Web.Hakyll

import Data.Traversable
import Data.List

import qualified Control.Monad.Reader as Reader

data HaapTable = HaapTable
    { haapTableTitle :: String
    , haapTableHeaders :: [String]
    , haapTableRows :: [[String]]
    , haapTablePath :: FilePath
    }

renderHaapTable :: HaapTable -> Haap p args db Hakyll FilePath
renderHaapTable t = do
    hakyllRules $ create [fromFilePath $ haapTablePath t] $ do
        route idRoute
        compile $ do
            let headerCtx = field "header" (return . itemBody)
            let colCtx = field "col" (return . itemBody)
                       `mappend` constField "class" ""
            let rowCtx = listFieldWith "cols" colCtx (mapM makeItem . itemBody)
            let pageCtx = constField "title" (haapTableTitle t)
                        `mappend` constField "projectpath" (fileToRoot $ haapTablePath t)
                        `mappend` listField "headers" headerCtx (mapM makeItem $ haapTableHeaders t)
                        `mappend` listField "rows" rowCtx (mapM makeItem $ haapTableRows t)
            makeItem "" >>= loadAndApplyTemplate "templates/table.html" pageCtx
    return (haapTablePath t)




