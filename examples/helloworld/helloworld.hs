{-# LANGUAGE EmptyDataDecls, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

{-|
Module      : Main
Description : The @helloworld@ HAAP example

This module presents a basic example of the usage of HAAP, consisting on testing an @HSpec@
specification and generating a webpage with the results with @Hakyll@.
-}
module Main where

import HAAP hiding (applyTemplate)

import Data.Default
import Data.Binary
import Data.Char
import Data.Monoid hiding ((<>))
import qualified Data.Map as Map
import Hakyll

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class

import System.Random.Shuffle
import System.Process

import GHC.Generics (Generic(..))

{-|
An HAAP script that runs tests with the @HSpec@ plugin and uses the @Hakyll@
plugin to generate a webpage with the results.
-}
main = do
    -- load the @Hakyll@ plugin with default arguments
    runHaap defaultProject $ useHakyll defaultHakyllArgs $ do
        let ordSpec = bounded "Int" [97,98,3,4] $ \x ->
              bounded "Char" "abcd" $ \y -> 
              testEqual x (ord y)
        -- run @HSpec@ and render the results in @spec.html@
        spec <- useSpec defaultHaapSpecArgs $ renderHaapSpec "spec.html" "" "ordtest" ordSpec
        -- create an @index.md@ webpage using the @Hakyll@ plugin
        hakyllRules $ do
            create ["index.md"] $ do
                route (setExtension "html")
                compile $ do
                    makeItem ("#Hello\n[ordtest](spec.html)"::String) >>= renderPandoc



