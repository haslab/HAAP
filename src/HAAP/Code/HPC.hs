module HAAP.Code.HPC where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils

import Data.Traversable
import Data.Foldable

import qualified Control.Monad.Reader as Reader

import System.FilePath

data HpcArgs args = HpcArgs
    { hpcExecutables :: [FilePath] -- executables to run with hpc
    , hpcGHC :: args -> GHCArgs
    , hpcIO :: args -> IOArgs
    , hpcSandbox :: Bool
    , hpcHtmlPath :: FilePath -- relative path to the project to store hpc results
    }

runHpc :: HpcArgs args -> Haap p args db a -> Haap p args db (a,Rules (),[FilePath])
runHpc hpc m = do
    ghc <- Reader.reader (hpcGHC hpc)
    let ghc' = ghc { ghcHpc = True }
    do
        forM (hpcExecutables hpc) $ \path -> do
            io <- Reader.reader (hpcIO hpc)
            let io' = io { ioSandbox = hpcSandbox hpc }
            let (dir,exec) = splitFileName path
            runSh $ do
                shCd dir
                shCompileWith io' ghc' [exec]
        x <- m
        htmls <- forM (hpcExecutables hpc) $ \path -> do
            io <- Reader.reader (hpcIO hpc)
            let io' = io { ioSandbox = hpcSandbox hpc }
            let (dir,exec) = splitFileName path
            let destdir = toRoot dir </> hpcHtmlPath hpc </> exec
            runSh $ do
                shCd dir
                shCommandWith io' "hpc" ["markup",exec,"--destdir="++destdir]
            return $ hpcHtmlPath hpc </> "hpc_index.html"
        let rules = do
            -- copy the hpc generated documentation
            forM_ htmls $ \html -> do
                match (fromGlob $ takeDirectory html </> "*") $ do
                    route   idRoute
                    compile copyFileCompiler
        return (x,rules,htmls)