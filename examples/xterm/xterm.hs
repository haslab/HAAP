{-# LANGUAGE EmptyDataDecls, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import HAAP

import Data.Default
import Data.Binary
import Data.Char
import Data.Monoid hiding ((<>))
import qualified Data.Map as Map

import Control.DeepSeq
import Control.Monad.IO.Class

import System.Random.Shuffle
import System.Process

import GHC.Generics (Generic(..))

example :: Project
example = Project
    { projectName = "xterm"
    , projectPath = "."
    , projectTmpPath = "tmp"
    , projectGroups = []
    , projectTasks = []
    }

main = do
    let cfg = HakyllArgs defaultConfiguration False True def
    runHaap example $ useHakyll cfg $ do
            
        useAndRunXterm exXterm
        
        hakyllRules $ do
                
            create ["index.md"] $ do
                route (setExtension "html")
                compile $ do
                    makeItem ("#Xterm example\n[run](Game.jsexe/run.html)"::String) >>= renderPandoc
        
        return ()

exXterm :: XtermArgs
exXterm = XtermArgs (Left "Game.hs") "Game" ghcjs def "web"
    where
    ghcjs = def { ghcjsSafe = False }

