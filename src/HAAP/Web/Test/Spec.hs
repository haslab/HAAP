{-# LANGUAGE OverloadedStrings #-}

module HAAP.Web.Test.Spec where

import HAAP.Web.Hakyll
import HAAP.Core
import HAAP.Utils
import HAAP.Test.Spec
import HAAP.Pretty
import HAAP.IO

import Data.Traversable

renderHaapSpecWith :: HakyllP -> (args -> IOArgs) -> (args -> HaapSpecArgs) -> FilePath -> String -> String -> HaapSpec -> Haap p args db Hakyll FilePath
renderHaapSpecWith hp getIOArgs getArgs path title notes spec = do
    test <- runSpecWith getIOArgs getArgs spec
    renderHaapTest hp path title notes test

renderHaapSpecsWith :: HakyllP -> (args -> IOArgs) -> (args -> HaapSpecArgs) -> FilePath -> String -> [(String,HaapSpec)] -> Haap p args db Hakyll FilePath
renderHaapSpecsWith hp getIOArgs getArgs path title specs = do
    tests <- forM specs $ mapSndM (runSpecWith getIOArgs getArgs)
    renderHaapTests hp path title tests

renderHaapTest :: HakyllP -> FilePath ->  String -> String -> HaapTestTableRes -> Haap p args db Hakyll FilePath
renderHaapTest hp path title notes spec = do
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

renderHaapTests :: HakyllP -> FilePath ->  String -> [(String,HaapTestTableRes)] -> Haap p args db Hakyll FilePath
renderHaapTests hp path title specs = do
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

