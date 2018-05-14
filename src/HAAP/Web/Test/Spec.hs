{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Spec@ plugin to run test specifications and generate HTML webpages with the results.
-}

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module HAAP.Web.Test.Spec where

import HAAP.Web.Hakyll
import HAAP.Core
import HAAP.Utils
import HAAP.Test.Spec
import HAAP.Pretty
import HAAP.IO
import HAAP.Plugin

import Control.Monad.IO.Class

import Data.Traversable

renderHaapSpec :: (MonadIO m,HasPlugin Hakyll t m,HasPlugin Spec t m) => FilePath -> String -> String -> HaapSpec -> Haap t m FilePath
renderHaapSpec path title notes spec = do
    test <- runSpec spec
    renderHaapTest path title notes test

renderHaapSpecs :: (MonadIO m,HasPlugin Hakyll t m,HasPlugin Spec t m) => FilePath -> String -> [(String,HaapSpec)] -> Haap t m FilePath
renderHaapSpecs path title specs = do
    tests <- forM specs $ mapSndM (runSpec)
    renderHaapTests path title tests

renderHaapTest :: (HasPlugin Hakyll t m,HasPlugin Spec t m) => FilePath ->  String -> String -> HaapTestTableRes -> Haap t m FilePath
renderHaapTest path title notes spec = do
    hp <- getHakyllP
    hakyllRules $ create [fromFilePath path] $ do
        route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
        compile $ do
            let classCtx i = case snd (itemBody i) of
                                HaapTestOk -> "hspec-success"
                                HaapTestError _ -> "hspec-failure"
                                HaapTestMessage _ -> "hspec-warning datatext"
            let headerCtx = field "header" (return . itemBody)
            let rowCtx =  field "result" (return . pretty . snd . itemBody)
                        `mappend` field "class" (return . classCtx)
                        `mappend` listFieldWith "cols" (field "col" (return . itemBody)) (\i -> mapM makeItem (fst $ itemBody i))
            let specCtx = constField "title" (title)
                         `mappend` listField "headers" headerCtx (mapM makeItem $ haapTestTableHeader spec)
                         `mappend` listField "rows" rowCtx (mapM makeItem $ haapTestTableRows spec)
                         `mappend` constField "projectpath" (fileToRoot $ hakyllRoute hp path)
                         `mappend` constField "notes" notes                        
            makeItem "" >>= loadAndApplyHTMLTemplate "templates/spec.html" specCtx >>= hakyllCompile hp
    return (hakyllRoute hp path)

renderHaapTests :: (HasPlugin Hakyll t m,HasPlugin Spec t m) => FilePath ->  String -> [(String,HaapTestTableRes)] -> Haap t m FilePath
renderHaapTests path title specs = do
    hp <- getHakyllP
    hakyllRules $ create [fromFilePath path] $ do
        route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
        compile $ do
            let classCtx i = case snd (itemBody i) of
                                HaapTestOk -> "hspec-success"
                                HaapTestError _ -> "hspec-failure"
                                HaapTestMessage _ -> "hspec-warning datatext"
            let headerCtx = field "header" (return . itemBody)
            let rowCtx =  field "result" (return . pretty . snd . itemBody)
                        `mappend` field "class" (return . classCtx)
                        `mappend` listFieldWith "cols" (field "col" (return . itemBody)) (\i -> mapM makeItem (fst $ itemBody i))
            let specCtx = field "name" (return . fst . itemBody)
                         `mappend` listFieldWith "headers" headerCtx (mapM makeItem . haapTestTableHeader . snd . itemBody)
                         `mappend` listFieldWith "rows" rowCtx (mapM makeItem . haapTestTableRows . snd . itemBody)
            let pageCtx = constField "title" title
                        `mappend` constField "projectpath" (fileToRoot $ hakyllRoute hp path)
                        `mappend` listField "specs" specCtx (mapM makeItem specs)
            makeItem "" >>= loadAndApplyHTMLTemplate "templates/specs.html" pageCtx >>= hakyllCompile hp
    return (hakyllRoute hp path)

