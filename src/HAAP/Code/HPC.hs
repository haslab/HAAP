module HAAP.Code.HPC where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils
import HAAP.Compiler.GHC
import HAAP.Web.HTML.Pandoc

import Data.Traversable
import Data.Foldable
import Data.Maybe

import Control.Monad
import qualified Control.Monad.Reader as Reader

import System.FilePath

data HpcArgs args = HpcArgs
    { hpcExecutable :: FilePath -- executables to run with hpc
    , hpcGHC :: args -> GHCArgs
    , hpcIO :: args -> IOArgs
    , hpcSandbox :: Maybe FilePath
    , hpcHtmlPath :: FilePath -- relative path to the project to store hpc results
    }

runHpc :: HakyllP -> HpcArgs args -> (IOResult -> Haap p args db Hakyll a) -> Haap p args db Hakyll (a,FilePath)
runHpc hp hpc m = do
    tmp <- getProjectTmpPath
    ghc <- Reader.reader (hpcGHC hpc)
    let ghc' = ghc { ghcHpc = True }
    io <- Reader.reader (hpcIO hpc)
    let (dir,exec) = splitFileName (hpcExecutable hpc)
    let io' = io { ioSandbox = fmap (dirToRoot dir </>) (hpcSandbox hpc) }
    do
        ghcres <- runSh $ do
            shCd dir
            res <- shGhcWith io' ghc' [exec]
            shRm $ addExtension exec "tix"
            return res
            
        x <- m ghcres
        
        let destdir = dirToRoot dir </> tmp </> hpcHtmlPath hpc </> exec
        let html = hpcHtmlPath hpc </> exec </> "hpc_index.html"
        orErrorWritePage (tmp </> html) mempty $ runSh $ do
            shCd dir
            shCommandWith io' "hpc" ["markup",exec,"--destdir="++destdir]
            
        hakyllRules $ do
            -- copy the hpc generated documentation
            match (fromGlob $ tmp </> hpcHtmlPath hpc </> exec </> "*") $ do
                route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                compile $ getResourceString >>= liftCompiler (asPandocHTML $ pandocChangeLinkUrls $ hakyllRoute hp) >>= hakyllCompile hp
        return (x,hakyllRoute hp $ html)