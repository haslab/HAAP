{-
HAAP: Haskell Automated Assessment Platform

This module provides documentation analysis functions by resorting to the _SourceGraph_ library (<https://hackage.haskell.org/package/SourceGraph>).

-}

{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables, DeriveGeneric #-}

module HAAP.Code.Analysis.SourceGraph where

import HAAP.IO
import HAAP.Core
import HAAP.Code.Haskell
import HAAP.Log

import Data.Default
import qualified Data.Text as Text
import Data.Traversable
import Data.List
import Data.Csv
import Data.Maybe
import qualified Data.Map as Map
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.BCC
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Basic
import Data.Graph.Analysis
import Data.Graph.Analysis.Algorithms
import Data.Graph.Analysis.Utils
import Data.Graph.Analysis.Types (graph)

import Control.Monad.IO.Class
import Control.DeepSeq
import Control.Monad
--import Control.Exception
import Control.Arrow(first)

import Language.Haskell.Exts as H

import System.FilePath

import Safe

import GHC.Generics (Generic)

-- sourcegraph
import Language.Haskell.SourceGraph.Analyse.Module
import Language.Haskell.SourceGraph.Analyse.Everything
import Language.Haskell.SourceGraph.Parsing
import Language.Haskell.SourceGraph.Parsing.Types as SG
import Language.Haskell.SourceGraph.Analyse.GraphRepr
import Language.Haskell.SourceGraph.Analyse.Utils

data SGReport = SGReport
    { maxModuleCC :: Int -- maximum cyclomatic complexity
    , globalCC :: Int -- global cyclomatic complexity
    , numInterModuleCalls :: Int -- number of inter-module function calls
    }
  deriving (Show,Generic)

instance NFData SGReport where

instance DefaultOrdered SGReport where
    headerOrder _ = header ["maxModuleCC","globalCC","numInterModuleCalls"]

instance ToNamedRecord SGReport where
    toNamedRecord (SGReport x x2 y) = namedRecord ["maxModuleCC" .= x,"globalCC" .= x2,"numInterModuleCalls" .= y]
instance FromNamedRecord SGReport where
    parseNamedRecord m = SGReport <$> m .: "maxModuleCC" <*> m .: "globalCC" <*> m .: "numInterModuleCalls"

instance Default SGReport where
    def = SGReport (-1) (-1) (-1)


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

runSourceGraph :: (MonadIO m,HaapStack t m) => Bool -> [FilePath] -> Haap t m SGReport
runSourceGraph ignoreDatas files = orLogDefault def $ do
    (_,ms) <- liftIO $ parseHaskellFiles files
    let ccs = map (cycleCompModule ignoreDatas) (Map.elems ms)
    let gcc = cycleCompModules ignoreDatas ms
    let mcalls = moduleCalls ms
    return $ SGReport (maximumDef 0 ccs) gcc mcalls

cycleCompModule :: Bool -> ParsedModule -> Int
cycleCompModule ignoreDatas m = cc
    where
    (_,(n,_,fd)) = moduleToGraph m
    ignores = if ignoreDatas
        then updateGraph (labfilter $ not . isData . eType)
        else id
    cc = cyclomaticComplexity $ ignores $ graphData $ collapsedHData fd

cycleCompModules :: Bool -> ParsedModules -> Int
cycleCompModules ignoreDatas m = cc
    where
    cd = codeToGraph (Map.keys m) m
    ignores = if ignoreDatas
        then updateGraph (labfilter $ not . isData . eType)
        else id
    cc = cyclomaticComplexity $ ignores $ graphData $ collapsedHData cd

cyclomaticComplexityGr :: DynGraph gr => gr a b -> Int
cyclomaticComplexityGr gd = e - n + 2*p
    where
      p = length $ componentsOf gd
      n = noNodes gd
      e = length $ labEdges gd

moduleCalls :: ParsedModules -> Int
moduleCalls ms = length es
    where
    mns = Map.keys ms
    g::AGr Entity CallType = graph $ graphData $ collapsedHData $ codeToGraph mns ms
    es = filter isInterModuleCall $ edges g
    isInterModuleCall (x,y) = case (lab g x,lab g y) of
        (Just x',Just y') -> inModule x' /= inModule y'
        otherwise -> False
    
-- * slicing

runSlice :: (MonadIO m,HaapStack t m) => [FilePath] -> (String -> Bool) -> (String -> Bool) -> Haap t m ([Decl SrcSpanInfo],Int,Int)
runSlice file = runSliceWith file Nothing (Just [])

runSliceWith :: (MonadIO m,HaapStack t m) => [FilePath] -> Maybe [Extension] -> Maybe [H.Fixity] -> (String -> Bool) -> (String -> Bool) -> Haap t m ([Decl SrcSpanInfo],Int,Int)
runSliceWith files ext fix isSlice excludeSlice = orLogDefault (def,-1,-1) $ do

    -- sourcegraph
    (_,ms) <- liftIO $ parseHaskellFiles files
    let (map SG.name -> slicednames,cc,noloops) = sliceEntities ms (isSlice . SG.name) (excludeSlice . SG.name)
    
    -- haskell-src-exts
    modules <- mapM (\f -> parseHaskellFileWith f ext fix) files
    let slices = sliceModules modules slicednames
    return (slices,cc,noloops)
    
sliceModules :: [H.Module SrcSpanInfo] -> [String] -> [H.Decl SrcSpanInfo]
sliceModules modules slicednames = decls'
    where
    decls = getTopDecls modules
    getDecl d = case getTopName d of
                      { Nothing -> Nothing
                      ; Just n -> if elem (nameString n) slicednames then Just d else Nothing
                      }
    decls' = catMaybes $ map getDecl decls

sliceEntities :: ParsedModules -> (Entity -> Bool) -> (Entity -> Bool) -> ([Entity],Int,Int)
sliceEntities ms isSlice excludeSlice = (map snd lslice,cc,length loops)
    where
    start_lnodes = filter (isSlice . snd) lnodes
    mns = Map.keys ms
    g::AGr Entity CallType = efilter (\(_,_,e) -> e == NormalCall) $ graph $ graphData $ collapsedHData $ codeToGraph mns ms
    lnodes = labNodes g
    slice = accessibleFrom g (map fst start_lnodes)
    lslice = filter (not . excludeSlice . snd) $ map (\i -> (i,fromJust $ lab g i)) slice
    subg = subgraph (map fst lslice) g
    cc = cyclomaticComplexityGr subg
    loops = subloops subg

subloops :: DynGraph gr => gr a b -> [gr a b]
subloops g = cycs
    where
    sccs = scc g
    cycs = filter hasCycle $ map (flip subgraph g) sccs
    hasCycle g = if noNodes g == 1 then hasLoop g else True


