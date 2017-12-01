{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}

module HAAP.Code.Analysis.SourceGraph where

import HAAP.IO
import HAAP.Core
import HAAP.Code.Haskell
import HAAP.Log

import Data.Default
import qualified Data.Text as Text
import Data.Traversable
import Data.Csv
import Data.Maybe
import qualified Data.Map as Map
import Data.Graph.Inductive.Graph
import Data.Graph.Analysis
import Data.Graph.Analysis.Types (graph)

import Control.Monad.IO.Class
import Control.DeepSeq
import Control.Monad
import Control.Exception

import System.FilePath

import Safe

import GHC.Generics (Generic)

-- sourcegraph
import Analyse.Module
import Analyse.Everything
import Parsing
import Parsing.Types
import Analyse.GraphRepr
import Analyse.Utils

data SGReport = SGReport
    { maxCC :: Int -- maximum cyclomatic complexity
    , numInterModuleCalls :: Int -- number of inter-module function calls
    }
  deriving (Show,Generic)

instance NFData SGReport where

instance DefaultOrdered SGReport where
    headerOrder _ = header ["maxCC","numInterModuleCalls"]

instance ToNamedRecord SGReport where
    toNamedRecord (SGReport x y) = namedRecord ["maxCC" .= x,"numInterModuleCalls" .= y]
instance FromNamedRecord SGReport where
    parseNamedRecord m = SGReport <$> m .: "maxCC" <*> m .: "numInterModuleCalls"

instance Default SGReport where
    def = SGReport (-1) (-1)


-- * tool

--runCComplexity :: HaapMonad m => IOArgs -> [FilePath] -> Haap p args db m CComplexity
--runCComplexity ioargs files = do
--    files' <- forM files $ \file -> orLogMaybe $ do
--        logEvent $ "parsing complexity for " ++ file
--        mn <- parseModuleFileName file
--        return (file,mn)
--    runCComplexity' ioargs (catMaybes files')
--
--runCComplexity' :: HaapMonad m => IOArgs -> [(FilePath,String)] -> Haap p args db m CComplexity
--runCComplexity' ioargs files = do
--    ccs <- liftM catMaybes $ mapM (orLogMaybe . cComplexity ioargs) files
--    let cc = if (length ccs > 0) then maximumDef 0 ccs else (-1)
--    return $ CComplexity cc
--
--cComplexity :: HaapMonad m => IOArgs -> (FilePath,String) -> Haap p args db m Int
--cComplexity ioargs (m,s) = do
--    runShWith (const ioargs) $ do
--        shCommandWith_ (ioargs) "SourceGraph" [m]
--        x <- shCommandWith (ioargs) "egrep" ["-w","<p>The cyclomatic complexity of "++s++" is:",takeDirectory m++"/SourceGraph/"++s++".html"]
--        let ax = drop (37 + (length s)) $ Text.unpack $ resStdout x
--        liftIO $ evaluate $ force $ readNote "cComplexity" $ take (length ax - 6) ax 

-- * library

runSourceGraph :: HaapMonad m => [FilePath] -> Haap p args db m SGReport
runSourceGraph files = orLogDefault def $ do
    (_,ms) <- liftIO $ parseHaskellFiles files
    let ccs = map cycleCompModule (Map.elems ms)
    let mcalls = moduleCalls ms
    return $ SGReport (maximumDef 0 ccs) mcalls

cycleCompModule :: ParsedModule -> Int
cycleCompModule m = cc
    where
    (_,(n,_,fd)) = moduleToGraph m
    cc = cyclomaticComplexity . graphData $ collapsedHData fd

moduleCalls :: ParsedModules -> Int
moduleCalls ms = length es
    where
    mns = Map.keys ms
    g::AGr Entity CallType = graph $ graphData $ collapsedHData $ codeToGraph mns ms
    es = filter isInterModuleCall $ edges g
    isInterModuleCall (x,y) = case (lab g x,lab g y) of
        (Just x',Just y') -> inModule x' /= inModule y'
        otherwise -> False
    







