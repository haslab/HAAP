{-
HAAP: Haskell Automated Assessment Platform

This module provides the @BinaryDB@ plugin that provides binary serialization @HaapDB@ functionalities via the _binary_ library (<https://hackage.haskell.org/package/binary>).

-}

{-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, EmptyDataDecls, TypeFamilies, InstanceSigs #-}

module HAAP.DB.Binary where

import HAAP.Core
import HAAP.DB
--import HAAP.Lens
import HAAP.IO
import HAAP.Plugin

import Data.Binary
import Data.Proxy

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Monad.Trans.Compose
import Control.Monad.Catch
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState(..),StateT(..))
import qualified Control.Monad.State as State

import System.FilePath

data BinaryDB st = BinaryDB st

--data BinaryDBDB st = BinaryDBDB st
data BinaryDBArgs st = BinaryDBArgs
    { binaryDBFile :: FilePath -- relative filepath
    , binaryDBInit :: st -- initial database
    , binaryDBIOArgs :: IOArgs 
    }

data BinaryDBQuery st a = BinaryDBQuery (st -> a)
data BinaryDBUpdate st a = BinaryDBUpdate (st -> (a,st))

instance Binary st => HaapPlugin (BinaryDB st) where
    type PluginI (BinaryDB st) = BinaryDBArgs st
    type PluginO (BinaryDB st) = ()
    type PluginT (BinaryDB st) = StateT (BinaryDB st)
    type PluginK (BinaryDB st) t m = (MonadIO m)

    usePlugin getArgs m = do
        path <- getProjectPath
        args <- getArgs
        let file = path </> binaryDBFile args
        st <- orDo (\err -> return $ binaryDBInit args) (runBaseIOWith (binaryDBIOArgs args) $ decodeFile file)
        let run (ComposeT m) = do
            ((b,(),w),BinaryDB db') <- State.runStateT m (BinaryDB st)
            return ((b,db'),(),w)
        (x,st') <- mapHaapMonad run m
        runBaseIOWith (binaryDBIOArgs args) $ encodeFile file st'
        return (x,())

useBinaryDB :: (Binary st,HaapStack t m,PluginK (BinaryDB st) t m) => (PluginI (BinaryDB st)) -> Haap (PluginT (BinaryDB st) :..: t) m a -> Haap t m a
useBinaryDB args = usePlugin_ (return args)

instance Binary st => HaapDB (BinaryDB st) where
    type DBQuery (BinaryDB st) a = BinaryDBQuery st a
    type DBUpdate (BinaryDB st) a = BinaryDBUpdate st a

    queryDB (BinaryDBQuery q::BinaryDBQuery st a) = do
        BinaryDB st <- liftPluginProxy (Proxy::Proxy (BinaryDB st)) $ State.get
        return $ q st
        
    updateDB (BinaryDBUpdate u::BinaryDBUpdate st a) = do
        BinaryDB st <- liftPluginProxy (Proxy::Proxy (BinaryDB st)) $ State.get
        let (x,st') = u st
        liftPluginProxy (Proxy::Proxy (BinaryDB st)) $ State.put $ BinaryDB st'
        return x

instance (HaapMonad m) => HasPlugin (BinaryDB st) (StateT (BinaryDB st)) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin (BinaryDB st) (ComposeT (StateT (BinaryDB st)) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m




