{-
HAAP: Haskell Automated Assessment Platform

This module provides the @AcidDB@ plugin that provides persistent state serialization @HaapDB@ functionalities with ACID guarantees via the _acid-db_ library (<https://hackage.haskell.org/package/acid-state>).

-}


{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses, EmptyDataDecls, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleContexts, RankNTypes, GADTs, TypeFamilies, InstanceSigs, ScopedTypeVariables, TemplateHaskell #-}

module HAAP.DB.Acid where

import HAAP.Core
import HAAP.DB
--import HAAP.DB.State
--import HAAP.Lens
import HAAP.IO
import HAAP.Plugin

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Trans
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer as Writer
import Control.Monad.State (MonadState(..),StateT(..))
import qualified Control.Monad.State as State

import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Data.Typeable

import System.FilePath

data AcidDB st

data AcidDBArgs st = AcidDBArgs
    { acidDBFile :: FilePath -- relative filepath
    , acidDBInit :: st -- initial database
    , acidDBIOArgs :: IOArgs -- timeout for acid-db operations
    }

data AcidDBQuery st a where
    AcidDBQuery :: (QueryEvent ev,EventState ev ~ st,EventResult ev ~ a) => ev -> AcidDBQuery st a

data AcidDBUpdate st a where
    AcidDBUpdate :: (UpdateEvent ev,EventState ev ~ st,EventResult ev ~ a) => ev -> AcidDBUpdate st a

instance IsAcidic st => HaapPlugin (AcidDB st) where
    type PluginI (AcidDB st) = AcidDBArgs st
    type PluginO (AcidDB st) = ()
    type PluginT (AcidDB st) = StateT (IOArgs,AcidState st)
    type PluginK (AcidDB st) t m = (MonadIO m)
    
    usePlugin getArgs m = do
        args <- getArgs
        path <- getProjectPath
        let ioargs = acidDBIOArgs args
        acid <- runBaseIOWith (ioargs) $ openLocalStateFrom (path </> acidDBFile args) (acidDBInit args)
        x <- mapHaapMonad (flip State.evalStateT (ioargs,acid) . unComposeT) m
        ignoreError $ runBaseIOWith (ioargs) $ createArchive acid
        ignoreError $ runBaseIOWith (ioargs) $ closeAcidState acid
        return (x,())

useAcidDB :: (IsAcidic st,HaapStack t m,PluginK (AcidDB st) t m) => (PluginI (AcidDB st)) -> Haap (PluginT (AcidDB st) :..: t) m a -> Haap t m a
useAcidDB args = usePlugin_ (return args)

instance IsAcidic st => HaapDB (AcidDB st) where
    type DBQuery (AcidDB st) a = AcidDBQuery st a
    type DBUpdate (AcidDB st) a = AcidDBUpdate st a
        
    queryDB (AcidDBQuery q) = do
        (ioargs,acid) <- liftPluginProxy (Proxy::Proxy (AcidDB st)) $ State.get
        runBaseIOWith (ioargs) $ query acid q
        
    updateDB (AcidDBUpdate u :: AcidDBUpdate st a) = do
        (ioargs,acid) <- liftPluginProxy (Proxy::Proxy (AcidDB st)) $ State.get
        runBaseIOWith (ioargs) $ update acid u

instance HaapMonad m => HasPlugin (AcidDB st) (StateT (IOArgs,AcidState st)) m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT (StateT (IOArgs, AcidState st)) m (t2 m)) => HasPlugin (AcidDB st) (ComposeT (StateT (IOArgs,AcidState st)) t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m
    
    
    
