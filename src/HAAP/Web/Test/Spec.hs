{-# LANGUAGE OverloadedStrings #-}

module HAAP.Web.Test.Spec where

import HAAP.Web.Hakyll
import HAAP.Core
import HAAP.Utils
import HAAP.Test.Spec
import HAAP.Pretty

import Data.Traversable

renderHaapSpecWith :: (args -> HaapSpecArgs) -> FilePath -> String -> String -> HaapSpec -> Haap p args db Hakyll FilePath
renderHaapSpecWith getArgs path title notes spec = do
    test <- runSpecWith getArgs spec
    renderHaapTest path title notes test

renderHaapSpecsWith :: (args -> HaapSpecArgs) -> FilePath -> String -> [(String,HaapSpec)] -> Haap p args db Hakyll FilePath
renderHaapSpecsWith getArgs path title specs = do
    tests <- forM specs $ mapSndM (runSpecWith getArgs)
    renderHaapTests path title tests

renderHaapTest :: FilePath ->  String -> String -> HaapTestTableRes -> Haap p args db Hakyll FilePath
renderHaapTest path title notes spec = do
    hakyllRules $ create [fromFilePath path] $ do
        route idRoute
        compile $ do
            let showRes Nothing = "OK"
                showRes (Just err) = pretty err
            let classCtx i = case snd (itemBody i) of { Nothing -> "hspec-success"; otherwise -> "hspec-failure"}
            let headerCtx = field "header" (return . itemBody)
            let rowCtx =  field "result" (return . showRes . snd . itemBody)
                        `mappend` field "class" (return . classCtx)
                        `mappend` listFieldWith "cols" (field "col" (return . itemBody)) (\i -> mapM makeItem (fst $ itemBody i))
            let specCtx = constField "title" (title)
                         `mappend` listField "headers" headerCtx (mapM makeItem $ haapTestTableHeader spec)
                         `mappend` listField "rows" rowCtx (mapM makeItem $ haapTestTableRows spec)
                         `mappend` constField "projectpath" (fileToRoot path)
                         `mappend` constField "notes" notes                        
            makeItem "" >>= loadAndApplyTemplate "templates/spec.html" specCtx
    return (path)

renderHaapTests :: FilePath ->  String -> [(String,HaapTestTableRes)] -> Haap p args db Hakyll FilePath
renderHaapTests path title specs = do
    hakyllRules $ create [fromFilePath path] $ do
        route idRoute
        compile $ do
            let showRes Nothing = "OK"
                showRes (Just err) = pretty err
            let classCtx i = case snd (itemBody i) of { Nothing -> "hspec-success"; otherwise -> "hspec-failure"}
            let headerCtx = field "header" (return . itemBody)
            let rowCtx =  field "result" (return . showRes . snd . itemBody)
                        `mappend` field "class" (return . classCtx)
                        `mappend` listFieldWith "cols" (field "col" (return . itemBody)) (\i -> mapM makeItem (fst $ itemBody i))
            let specCtx = field "name" (return . fst . itemBody)
                         `mappend` listFieldWith "headers" headerCtx (mapM makeItem . haapTestTableHeader . snd . itemBody)
                         `mappend` listFieldWith "rows" rowCtx (mapM makeItem . haapTestTableRows . snd . itemBody)
            let pageCtx = constField "title" title
                        `mappend` constField "projectpath" (fileToRoot path)
                        `mappend` listField "specs" specCtx (mapM makeItem specs)
            makeItem "" >>= loadAndApplyTemplate "templates/specs.html" pageCtx
    return (path)

