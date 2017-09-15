{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, RankNTypes #-}

module HAAP.Sources where

import HAAP.Core
import HAAP.Lens

import Data.Traversable
import Data.Default
import Data.Map (Map(..))
import qualified Data.Map as Map

class HaapSource s where
    type Source s = r | r -> s
    type SourceInfo s = r | r -> s
    type SourceArgs s = r | r -> s
    getSourceWith :: (args -> SourceArgs s) -> Source s -> Haap p args db ()
    putSourceWith :: (args -> SourceArgs s) -> Source s -> Haap p args db ()
    getSourceInfoWith  :: (args -> SourceArgs s) -> Source s -> Haap p args db (SourceInfo s)

-- pulls the latest source (a.k.a. git pull)
getSource :: HaapSource s => Source s -> Haap p (SourceArgs s) db ()
getSource = getSourceWith id

-- pushes a new source (a.k.a git push)
putSource :: HaapSource s => Source s -> Haap p (SourceArgs s) db ()
putSource = putSourceWith id

getSourceInfo :: HaapSource s => Source s -> Haap p (SourceArgs s) db (SourceInfo s)
getSourceInfo = getSourceInfoWith id

