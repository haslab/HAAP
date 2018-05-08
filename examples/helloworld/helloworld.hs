{-# LANGUAGE EmptyDataDecls, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

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

main = do
    runHaap defaultProject $ useHakyll defaultHakyllArgs $ do
        let ordSpec = bounded "Int" [97,98,3,4] $ \x ->
              bounded "Char" "abcd" $ \y -> 
              testEqual x (ord y)
        spec <- useSpec defaultHaapSpecArgs $ renderHaapSpec "spec.html" "" "ordtest" ordSpec
        
        hakyllRules $ do
            create ["index.md"] $ do
                route (setExtension "html")
                compile $ do
                    makeItem ("#Hello\n[ordtest](spec.html)"::String) >>= renderPandoc



