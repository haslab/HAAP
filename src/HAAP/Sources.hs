{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, RankNTypes #-}

module HAAP.Sources where

import HAAP.Core
import HAAP.Lens

import Data.Traversable
import Data.Default
import Data.Map (Map(..))
import qualified Data.Map as Map

type SourceState s = Map (Source s) (SourceInfo s)

class IsSource s where
    type Source s = r | r -> s
    type SourceInfo s = r | r -> s
    type SourceArgs s = r | r -> s
    getSourceWith :: (args -> SourceArgs s) -> Lens'' db (SourceState s)
                  -> Source s -> Haap p args db (SourceInfo s)
    putSourceWith :: (args -> SourceArgs s) -> Lens'' db (SourceState s)
                  -> Source s -> Haap p args db (SourceInfo s)
    defaultSourceArgs :: SourceArgs s

-- updates to the latest source
getSource :: IsSource s => Source s -> Haap p (SourceArgs s) (SourceState s) (SourceInfo s)
getSource = getSourceWith id idLens''

putSource :: IsSource s => Source s -> Haap p (SourceArgs s) (SourceState s) (SourceInfo s)
putSource = putSourceWith id idLens''

-- for each group, update its source and run some function
forSource :: IsSource s => (Source s -> SourceInfo s -> Haap p (SourceArgs s) (SourceState s) r) -> Haap p (SourceArgs s) (SourceState s) [r]
forSource = forSourceWith id idLens''

forSourceWith :: IsSource s => (args -> SourceArgs s) -> Lens'' db (SourceState s)
              -> (Source s -> SourceInfo s -> Haap p args db r) -> Haap p args db [r]
forSourceWith fargs ldb upd = do
    db <- getStateLens'' ldb
    forM (Map.toList db) (uncurry upd)