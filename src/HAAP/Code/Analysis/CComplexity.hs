module HAAP.Code.Analysis.CComplexity where

import HAAP.IO
import HAAP.Core
import HAAP.Code.Haskell

import Data.Default
import qualified Data.Text as Text
import Data.Traversable

import System.FilePath

data CComplexity = CComplexity
    { maxCC :: Int -- maximum cyclomatic complexity
    }

instance Default CComplexity where
    def = CComplexity (-1)

runCComplexity :: HaapMonad m => [FilePath] -> Haap p args db m CComplexity
runCComplexity files = do
    files' <- forM files $ \file -> do
        mn <- parseModuleFileName file
        return (file,mn)
    runCComplexity' files'

runCComplexity' :: HaapMonad m => [(FilePath,String)] -> Haap p args db m CComplexity
runCComplexity' files = do
    ccs <- mapM cComplexity files
    let cc = if (length ccs > 0) then maximum ccs else (-1)
    return $ CComplexity cc

cComplexity :: HaapMonad m => (FilePath,String) -> Haap p args db m Int
cComplexity (m,s) = orDefault (-1) $ do
    let ioargs = def
    runShWith (const ioargs) $ do
        shCommandWith_ (ioargs) "SourceGraph " [m]
        x <- shCommandWith (ioargs) "egrep" ["-w","<p>The cyclomatic complexity of "++s++" is:",takeDirectory m++"/SourceGraph/"++s++".html"]
        let ax = drop (37 + (length s)) $ Text.unpack $ resStdout x
        return $ read $ take (length ax - 6) ax 