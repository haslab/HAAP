{- |
   Module      : Data.Graph.Analysis.Utils
   Description : Utility functions
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines various utility functions used throughout.
 -}
module Data.Graph.Analysis.Utils
    ( -- * Graph functions
      -- ** Data extraction
      -- $extracting
      node,         -- Re-exported from Internal
      label,        -- Re-exported from Internal
      labels,
      edge,
      eLabel,
      addLabels,    -- Re-exported from Internal
      addLabels',   -- Re-exported from Internal
      getLabels,    -- Re-exported from Internal
      getLabels',   -- Re-exported from Internal
      filterNodes,  -- Re-exported from Internal
      filterNodes', -- Re-exported from Internal
      pathValues,
      -- ** Graph manipulation
      undir,
      oneWay,
      mkSimple,
      compact,
      compact',
      compactSame,
      nlmap,
      delLNodes,
      -- ** Graph layout
      -- $spatial
      toPosGraph,
      getPositions,
      -- ** Cluster functions
      -- $cluster
      createLookup,
      setCluster,
      reCluster,
      reClusterBy,
      clusterCount,
      -- * List functions
      -- $list
      single,
      longerThan,
      addLengths,
      longest,
      lengthSort,
      groupElems,
      sortMinMax,
      shuffle,
      -- * Statistics functions
      mean,
      statistics,
      statistics',
      -- * Other functions
      fixPoint,
      fixPointGraphs,
      fixPointBy
    ) where

import Data.Graph.Analysis.Internal
import Data.Graph.Analysis.Types

import Data.Graph.Inductive.Graph
import Data.GraphViz( dotizeGraph
                    , GraphvizParams(isDirected)
                    , nonClusteredParams)
import Data.GraphViz.Attributes.Complete( Attribute(..)
                                        , Pos(..)
                                        , Point(..))

import Data.List(nub, nubBy, (\\), find, sort, sortBy, group, groupBy)
import Data.Maybe(fromJust)
import Data.Function(on)
import qualified Data.Set as Set
import qualified Data.IntMap as IMap
import Data.IntMap(IntMap)
import Control.Arrow(first, second)
import System.Random(RandomGen, randomR)

-- -----------------------------------------------------------------------------

-- $extracting Extracting data from graphs.

-- | The labels of all nodes in a tree.
labels :: (Graph g) => g a b -> [a]
labels = map label . labNodes

-- | Extract the 'Edge' from the 'LEdge'.
edge           :: LEdge b -> Edge
edge (n1,n2,_) = (n1,n2)

-- | The label of an 'LEdge'.
eLabel         :: LEdge b -> b
eLabel (_,_,b) = b

-- | Extract the actual 'LNode's from an 'LPath'.
pathValues          :: LPath a -> [LNode a]
pathValues (LP lns) = lns

-- -----------------------------------------------------------------------------

-- Manipulating graphs.

{- |
   Make the graph undirected, i.e. for every edge from A to B, there
   exists an edge from B to A.  The provided function
   'Data.Graph.Inductive.Basic.undir' duplicates loops as well, which
   isn't wanted.  It is assumed that no edges are already duplicates
   [i.e. if there exists an edge (n1,n2), then there doesn't exist
   (n2,n1)].  This function also preserves edge labels: if two edges
   exist between two nodes with different edge labels, then both edges
   will be duplicated.
-}
undir :: (Eq b, DynGraph gr) => gr a b -> gr a b
undir = gmap dupEdges
    where
      dupEdges (p,n,l,s) = (ps',n,l,ps)
          where
            ps = nub $ p ++ s
            ps' = filter (not . isLoop) ps
            isLoop (_,n') = n == n'

-- | This is a pseudo-inverse of 'undir': any edges that are both successor
--   and predecessor become successor edges only.
oneWay :: (DynGraph g, Eq b) => g a b -> g a b
oneWay = gmap rmPre
    where
      rmPre (p,n,l,s) = (p \\ s,n,l,s)

-- | Makes the graph a simple one, by removing all duplicate edges and loops.
--   The edges removed if duplicates exist are arbitrary.
mkSimple :: (DynGraph gr) => gr a b -> gr a b
mkSimple = gmap simplify
    where
      rmLoops n = filter ((/=) n . snd)
      rmDups = nubBy ((==) `on` snd)
      simpleEdges n = rmDups . rmLoops n
      simplify (p,n,l,s) = (p',n,l,s')
          where
            p' = simpleEdges n p
            s' = simpleEdges n s

-- | Adjoin duplicate edges by grouping the labels together.
compact :: (DynGraph gr) => gr a b -> gr a [b]
compact = gmap cmpct
    where
      cEs = map (swap . second (map fst))
            . groupElems snd
      cmpct (p,n,l,s) = (cEs p, n, l, cEs s)

-- | Compact the graph by counting how many multiple edges there are
--   (considering only the two nodes and not the labels).
compact' :: (DynGraph gr) => gr a b -> gr a Int
compact' = emap length . compact

-- | Compact the graph by adjoining identical duplicate edges.
compactSame :: (Ord b, DynGraph gr) => gr a b -> gr a (Int,b)
compactSame = gmap cmpct
    where
      cEs = map toAdj . group . sort
      toAdj as = let (l,n) = head as in ((length as,l),n)
      cmpct (p,n,l,s) = (cEs p, n, l, cEs s)

-- | Map over the labels on the nodes, using the node values as well.
nlmap   :: (DynGraph gr) => (LNode a -> c) -> gr a b -> gr c b
nlmap f = gmap f'
    where
      f' (p,n,l,s) = (p,n,f (n,l),s)

-- | Delete these labelled nodes from the graph.
delLNodes :: (DynGraph gr) => LNGroup a -> gr a b -> gr a b
delLNodes = delNodes . map fst

-- -----------------------------------------------------------------------------

{- $spatial
   Spatial positioning of graphs.  Use the 'dotizeGraph' function in
   "Data.GraphViz" to determine potential graph layouts.
-}

-- | Convert the graph into one with positions stored in the node
--   labels.  The 'Bool' parameter denotes if the graph is directed or
--   not.
toPosGraph     :: (DynGraph gr, Ord b) => Bool -> gr a b -> gr (PosLabel a) b
toPosGraph dir = nlmap getPos . emap rmAttrs . dotizeGraph params
    where
      params = nonClusteredParams{ isDirected = dir }
      rmAttrs = snd
      isPoint Pos{} = True
      isPoint _     = False
      getPos (n,(as,l)) = PLabel { xPos   = round $ xCoord pnt
                                 , yPos   = round $ yCoord pnt
                                 , pnode  = n
                                 , plabel = l
                                 }
          where
            -- Also assuming that we're not dealing with a
            -- spline-type point.
            (Pos (PointPos pnt)) = fromJust $ find isPoint as

-- | Returns the positions of the nodes in the graph, as found using
--   Graphviz.  The 'Bool' parameter denotes if the graph is directed
--   or not.
getPositions     :: (DynGraph gr, Ord b) => Bool -> gr a b -> [PosLabel a]
getPositions dir = map label . labNodes . toPosGraph dir

-- -----------------------------------------------------------------------------

-- $cluster Cluster utility functions.

-- | Create a cluster-lookup 'IntMap'.
createLookup :: [[Node]] -> IntMap Int
createLookup = IMap.fromList . concatMap addCluster . zip [1..] . lengthSort
    where
      addCluster (k,ns) = map (flip (,) k) ns

-- | Used when the clusters are assigned in a lookup 'IntMap' instance.
setCluster   :: (DynGraph gr) => IntMap Int -> gr a b -> gr (GenCluster a) b
setCluster m = nlmap assClust
    where
      assClust (n,l) = GC (m IMap.! n) l

-- | Change the cluster values in the graph by having the largest cluster
--   have the smallest cluster label.
reCluster   :: (DynGraph g) => g (GenCluster a) b -> g (GenCluster a) b
reCluster g = reClusterBy cs' g
    where
      cnts = IMap.toList $ clusterCount g
      cPop = map fst $ sortBy (flip compare `on` snd) cnts
      cs' = IMap.fromList $ zip cPop [1..]

-- | Change the cluster values using the given lookup 'IntMap'.
reClusterBy   :: (DynGraph g) => IntMap Int -> g (GenCluster a) b
              -> g (GenCluster a) b
reClusterBy m = nmap newClust
    where
      newClust c = c { clust = m IMap.! clust c }

-- | Create an 'IntMap' of the size of each cluster.
clusterCount :: (Graph g) => g (GenCluster a) b -> IntMap Int
clusterCount = ufold incMap IMap.empty
    where
      incMap (_,_,l,_) = IMap.insertWith ins (clust l) 1
      ins _ c = c + 1

-- -----------------------------------------------------------------------------

-- $list List utility functions.

-- | Return true if and only if the list contains a single element.
single     :: [a] -> Bool
single [_] = True
single  _  = False

-- | If we need to only tell if the list contains more than @n@ elements,
--   there's no need to find its length.
longerThan   :: Int -> [a] -> Bool
longerThan n = not . null . drop n

-- | Add the length of each sublist.
addLengths :: [[a]] -> [(Int,[a])]
addLengths = map ( \ as -> (length as, as))

-- | Returns the longest list in a list of lists.
longest :: [[a]] -> [a]
longest = head . lengthSort

lengthSort :: [[a]] -> [[a]]
lengthSort = map snd . sortBy (flip compare `on` fst) . addLengths

-- | Group elements by the given grouping function.
groupElems   :: (Ord b) => (a -> b) -> [a] -> [(b,[a])]
groupElems f = map createGroup
               . groupBy ((==) `on` fst)
               . sortBy (compare `on` fst)
               . map addOrd
    where
      addOrd a = (f a, a)
      createGroup bas@((b,_):_) = (b, map snd bas)
      -- This shouldn't ever happen, but let's suppress the -Wall warning.
      createGroup []            = error "Grouping resulted in an empty list!"

-- | Returns the unique elements of the list in ascending order,
--   as well as the minimum and maximum elements.
sortMinMax    :: (Ord a) => [a] -> ([a],a,a)
sortMinMax as = (as',aMin,aMax)
    where
      aSet = Set.fromList as
      as' = Set.toAscList aSet
      aMin = Set.findMin aSet
      aMax = Set.findMax aSet


{- |
   Shuffle a list of elements.
   This isn't the most efficient version, but should serve for small lists.
   Adapted from:
   <http://www.cse.unsw.edu.au/~tsewell/shuffle.html>
   The adaptation mainly involved altering the code so that the new
   random seed is also returned.
 -}
shuffle       :: (RandomGen g) => g -> [a] -> ([a],g)
shuffle g []  = ([],g)
shuffle g [x] = ([x],g)
shuffle g xs  = randomMerge g'' ((shYs,yn),(shZs,zn))
    where
        ((ys, yn), (zs, zn)) = splitAndCount xs (([], 0), ([], 0))
        (shYs,g') = shuffle g ys
        (shZs,g'') = shuffle g' zs

splitAndCount :: [a] -> (([a], Int), ([a], Int)) -> (([a], Int), ([a], Int))
splitAndCount [] result = result
splitAndCount (x : xs) ((ys, yn), (zs, zn)) =
    splitAndCount xs ((x : zs, zn + 1), (ys, yn))

{-
  Taken from the original site:

  The idea is to merge two shuffled lists which come with given sizes.
  If the lists X and Y have sizes n and m, we should pick the first element
  of X with probability n / n + m and the first element of Y with probability
  m / n + m. As X and Y are shuffled, picking the first element is random
  among their original elements, and thus this constitutes a random choice
  of first element from the original set.
 -}
randomMerge :: (RandomGen g) => g -> (([a], Int), ([a], Int)) -> ([a],g)
randomMerge g (([],_),(ys,_))       = (ys,g)
randomMerge g ((xs,_),([],_))       = (xs,g)
randomMerge g ((x:xs,xn),(y:ys,yn)) = if n <= xn
                                      then first (x:) xg
                                      else first (y:) yg
    where
      xg = randomMerge g' ((xs, xn - 1), (y : ys, yn))
      yg = randomMerge g' ((x : xs, xn), (ys, yn - 1))
      (n, g') = randomR (1, xn + yn) g

-- -----------------------------------------------------------------------------

-- Statistics functions.

-- | An efficient mean function by Don Stewart, available from:
--   <http://cgi.cse.unsw.edu.au/~dons/blog/2008/05/16#fast>
mean :: [Double] -> Double
mean = go 0 0
    where
      go :: Double -> Int -> [Double] -> Double
      go s l []     = s / fromIntegral l
      go s l (x:xs) = go (s+x) (l+1) xs

-- | Calculate the mean and standard deviation of a list of elements.
statistics    :: [Double]
              -> (Double,Double) -- ^ (Mean, Standard Deviation)
statistics as = (av,stdDev)
    where
      av = mean as
      stdDev = sqrt . mean $ map (sq . subtract av) as

-- | Calculate the mean and standard deviation of a list of 'Int' values.
statistics'    :: [Int]
               -> (Int,Int) -- ^ (Mean, Standard Deviation)
statistics' as = (av', stdDev')
    where
      (av,stdDev) = statistics $ map fromIntegral as
      av' = round av
      stdDev' = round stdDev

-- -----------------------------------------------------------------------------

-- Other utility functions.

-- | Find the fixed point of a function with the given initial value.
fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint = fixPointBy (==)

-- | Find the fixed point of a function with the given initial value,
--   using the given equality function.
fixPointBy       :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixPointBy eq f x = if eq x x'
                    then x'
                    else fixPointBy eq f x'
    where
      x' = f x
-- | Find the fixed point of a graph transformation function.
fixPointGraphs :: (Eq a, Eq b, Graph g) => (g a b -> g a b) -> g a b -> g a b
fixPointGraphs = fixPointBy equal
