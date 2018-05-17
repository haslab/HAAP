{-# LANGUAGE FlexibleContexts, EmptyDataDecls, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import HAAP
import HAAP.Utils

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

import Data.String
import qualified Data.Text as T

import Control.Monad 
import Control.Exception
import Control.Monad.Trans
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int64)
import Data.Word (Word, Word64)
import Data.Maybe (fromJust)
import Data.Traversable (mapM)
import Safe


import Control.Monad (unless)

import System.Directory
import System.FilePath
import Control.DeepSeq

example :: Project
example = Project
    { projectName = "gameworker"
    , projectPath = "."
    , projectTmpPath = "tmp"
    , projectGroups = []
    , projectTasks = []
    }

main = do
    let cfg = HakyllArgs defaultConfiguration False True def
    runHaap example $ useHakyll cfg $ do
        
        logEvent "Compiling Workers"
        js <- usePlugin_ (return $ GHCJSArgs False ["-ili1g100:li1g147:li1g175"] def) $ do
            js2 <- runGhcjs "Worker2.hs" "gameworker"
            js3 <- runGhcjs "Worker3.hs" "gameworker"
            js4 <- runGhcjs "Worker4.hs" "gameworker"
            return [js2,js3,js4]
        
        logEvent "Compiling Game"
        runBaseIO' $ do
            writeFile ("Game.hs") $
                "module Main where\n"++
                "import GameB\n"++
                "main = mainGameB "++show (map (\js -> dirToRoot "gameworker/Game.jsexe" </> js) js) ++ "\n"
        
        cw <- exCodeWorld >>= useAndRunCodeWorld
        
        hakyllRules $ do
            create ["index.md"] $ do
                route (setExtension "html")
                compile $ do
                    makeItem ("#Game and JavaScript workers Example\n[play](gameworker/Game.jsexe/run.html)"::String) >>= renderPandoc
        
        return ()

exCodeWorld :: HasPlugin Hakyll t IO => Haap t IO CodeWorldArgs
exCodeWorld = do
    cwImgs <- runBaseSh $ shFindWhen (\x -> takeExtension x == ".bmp") "graphics"
    hakyllRules $ forM_ cwImgs $ \cwImg -> do
        match (fromGlob cwImg) $ do
            route idRoute
            compile copyFileCompiler
    let ghcjs = def { ghcjsSafe = False }
    return $ CodeWorldArgs (Left "Game.hs") "Game" (CWGame CWGameConsole) ghcjs def "gameworker" (map (\x -> (x,x)) cwImgs) []

