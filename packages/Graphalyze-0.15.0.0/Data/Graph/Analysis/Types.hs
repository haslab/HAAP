{-# LANGUAGE TypeFamilies
            , FlexibleContexts
            , TypeSynonymInstances
            , FlexibleInstances
 #-}

{- |
   Module      : Data.Graph.Analysis.Types
   Description : Graphalyze Types and Classes
   Copyright   : (c) Ivan Lazar Miljenovic 2009
   License     : 2-Clause BSD
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the various types and classes utilised
   by the Graphalyze library.
 -}
module Data.Graph.Analysis.Types
    ( -- * Graph specialization.
      GraphData(..),
      AGr,
      Rel,
      NGroup,
      LNGroup,
      -- * Functions on @GraphData@.
      wantedRoots,
      addRoots,
      addRootsBy,
      applyAlg,
      applyDirAlg,
      mergeUnused,
      removeUnused,
      updateGraph,
      updateGraph',
      mapAllNodes,
      mapNodeType,
      -- * Clustering graphs based on their node labels.
      ClusterLabel(..),
      GraphID(..),
      -- * Graph label types.
      GenCluster(..),
      PosLabel(..)
    ) where

import Data.Graph.Analysis.Internal

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.GraphViz.Types(GraphID(..), ToGraphID(..))

import qualified Data.Set as S
import Data.Set(Set)

-- -----------------------------------------------------------------------------

-- | Represents information about the graph being analysed.
data GraphData n e = GraphData { -- | We use a graph type with no edge labels.
                                 graph :: AGr n e,
                                 -- | The expected root nodes in the graph.
                                 wantedRootNodes :: NGroup,
                                 -- | Is the data this graph represents
                                 --   directed in nature?
                                 directedData :: Bool,
                                 -- | Unused relationships (i.e. not in
                                 --   the actual graph).  These are the
                                 --   edges containing nodes not in the
                                 --   graph.
                                 unusedRelationships :: [Rel n e]
                               }
                     deriving (Eq, Show, Read)

-- | The expected roots in the data to be analysed.
wantedRoots    :: GraphData n e -> LNGroup n
wantedRoots gd = addLabels g rs'
    where
      g = graph gd
      gns = S.fromList $ nodes g
      rs = S.fromList $ wantedRootNodes gd
      rs' = S.toList $ gns `S.intersection` rs

-- | Add extra expected root nodes.  No checks are made that these
--   are valid 'Node' values.
addRoots      :: GraphData n e -> NGroup -> GraphData n e
addRoots gd ns = gd { wantedRootNodes = S.toList rs' }
    where
      ns' = S.fromList ns
      rs = S.fromList $ wantedRootNodes gd
      rs' = rs `S.union` ns'

-- | Use a filtering function to find extra root nodes to add.
addRootsBy      :: (LNode n -> Bool) -> GraphData n e -> GraphData n e
addRootsBy p gd = addRoots gd rs'
    where
      p' _ = p
      rs' = map node $ applyAlg (filterNodes p') gd

-- | Apply an algorithm to the data to be analysed.
applyAlg   :: (AGr n e -> a) -> GraphData n e -> a
applyAlg f = f . graph

-- | Apply an algorithm that requires knowledge about whether the
--   graph is directed ('True') or undirected ('False') to the data to
--   be analysed.
applyDirAlg     :: (Bool -> AGr n e -> a) -> GraphData n e -> a
applyDirAlg f g = f (directedData g) (graph g)

-- | Apply a function to all the data points.
--   This might be useful in circumstances where you want to reduce
--   the data type used to a simpler one, etc.  The function is also
--   applied to the datums in 'unusedRelationships'.
mapAllNodes      :: (Ord a, Ord e, Ord b) => (a -> b)
                    -> GraphData a e -> GraphData b e
mapAllNodes f gd = gd { graph = nmap f $ graph gd
                      , unusedRelationships = map (applyNodes f)
                                              $ unusedRelationships gd
                      }

-- | Apply the first function to nodes in the graph, and the second
--   function to those unknown datums in 'unusedRelationships'.
--   As a sample reason for this function, it can be used to apply a
--   two-part constructor (e.g. 'Left' and 'Right' from 'Either') to
--   the nodes such that the wanted and unwanted datums can be
--   differentiated before calling 'mergeUnused'.
mapNodeType          :: (Ord a, Ord b, Ord e) => (a -> b) -> (a -> b)
                        -> GraphData a e -> GraphData b e
mapNodeType fk fu gd = gd { graph = nmap fk $ graph gd
                          , unusedRelationships = map (applyNodes f)
                                                  $ unusedRelationships gd
                          }
    where
      knownNs = knownNodes gd
      f n = if S.member n knownNs
            then fk n
            else fu n

-- | Merge the 'unusedRelationships' into the graph by adding the
--   appropriate nodes.
mergeUnused    :: (Ord n, Ord e) => GraphData n e -> GraphData n e
mergeUnused gd = gd { graph = insEdges es' gr'
                    , unusedRelationships = []
                    }
    where
      gr = graph gd
      unRs = unusedRelationships gd
      mkS f = S.fromList $ map f unRs
      unNs = S.toList
             . flip S.difference (knownNodes gd)
             $ S.union (mkS fromNode) (mkS toNode)
      ns' = newNodes (length unNs) gr
      gr' = flip insNodes gr $ zip ns' unNs
      -- Should no longer contain any unused rels.
      es' = snd $ relsToEs (directedData gd)
                           (labNodes gr)
                           unRs

knownNodes :: (Ord n) => GraphData n e -> Set n
knownNodes = S.fromList . map snd . labNodes . graph

-- | Used to set @'unusedRelationships' = []@.  This is of use when
--   they are unneeded or because there is no sensible mapping
--   function to use when applying a mapping function to the nodes in
--   the graph.
removeUnused   :: GraphData n e -> GraphData n e
removeUnused g = g { unusedRelationships = [] }

-- | Replace the current graph by applying a function to it.  To
--   ensure type safety, 'removeUnused' is applied.
updateGraph     :: (AGr a b -> AGr c d)
                   -> GraphData a b -> GraphData c d
updateGraph f g = g { graph = applyAlg f g
                    , unusedRelationships = []
                    }

-- | Replace the current graph by applying a function to it, where the
--   function depends on whether the graph is directed ('True') or
--   undirected ('False').  To ensure type safety, 'removeUnused' is
--   applied.
updateGraph'     :: (Bool -> AGr a b -> AGr c d)
                    -> GraphData a b -> GraphData c d
updateGraph' f g = g { graph = applyDirAlg f g
                     , unusedRelationships = []
                     }


-- | An alias for the type of graph being used by default.
type AGr n e = Gr n e

-- | A grouping of 'Node's.
type NGroup = [Node]

-- | A grouping of 'LNode's.
type LNGroup a = [LNode a]

-- -----------------------------------------------------------------------------

-- | These types and classes represent useful label types.

-- | The class of outputs of a clustering algorithm.  This class is
--   mainly used for visualization purposes, with the 'Ord' instance
--   required for grouping.  Instances of this class are intended for
--   use as the label type of graphs.
class (Ord (Cluster cl), ToGraphID (Cluster cl)) => ClusterLabel cl where
    type Cluster cl
    type NodeLabel cl

    -- | The cluster the node label belongs in.
    cluster   :: cl -> Cluster cl

    -- | The actual label.
    nodeLabel :: cl -> NodeLabel cl

-- | A generic cluster-label type.
data GenCluster a = GC { clust :: Int
                       , nLbl  :: a
                       }
                    deriving (Eq,Show)

instance ClusterLabel (GenCluster a) where
    type Cluster (GenCluster a) = Int
    type NodeLabel (GenCluster a) = a

    cluster = clust
    nodeLabel = nLbl

-- | Label type for storing node positions.  Note that this isn't an
--   instance of 'ClusterLabel' since there's no clear indication on
--   which cluster a node belongs to at this stage.
data PosLabel a = PLabel { xPos   :: Int
                         , yPos   :: Int
                         , pnode  :: Node
                         , plabel :: a
                         }
                  deriving (Eq, Show)
