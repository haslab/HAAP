{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, EmptyDataDecls, TypeFamilies, InstanceSigs #-}

module HAAP.DB.State where

import HAAP.Core
import HAAP.DB
import HAAP.Plugin
--import HAAP.Lens

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState(..),StateT(..))
import qualified Control.Monad.State as State

import Data.Proxy

data StateDB st = StateDB st

data StateDBQuery st a = StateDBQuery (st -> a)
data StateDBUpdate st a = StateDBUpdate (st -> (a,st))

instance HaapPlugin (StateDB st) where
    type PluginI (StateDB st) = StateDB st
    type PluginO (StateDB st) = StateDB st
    type PluginT (StateDB st) = StateT (StateDB st)
    type PluginK (StateDB st) t m = ()

    usePlugin getDB m = do
        db <- getDB
        let run (ComposeT m) = do
            (e,db') <- State.runStateT m db
            case e of
                Left err -> return $ Left err
                Right (b,(),w) -> return $ Right ((b,db'),(),w)
        (x,db') <- mapHaapMonad run m
        return (x,db')

useStateDB :: (HaapStack t m,PluginK (StateDB st) t m) => (PluginI (StateDB st)) -> Haap (PluginT (StateDB st) :..: t) m a -> Haap t m (a,StateDB st)
useStateDB args = usePlugin (return args)

instance HaapDB (StateDB st) where
    type DBQuery (StateDB st) a = StateDBQuery st a
    type DBUpdate (StateDB st) a = StateDBUpdate st a
        
    queryDB (StateDBQuery q) = do
        StateDB st <- liftPluginProxy (Proxy::Proxy (StateDB st)) $ State.get
        return $ q st
        
    updateDB (StateDBUpdate u) = do
        StateDB st <- liftPluginProxy (Proxy::Proxy (StateDB st)) $ State.get
        let (x,st') = u st
        liftPluginProxy (Proxy::Proxy (StateDB st)) $ State.put $ StateDB st'
        return x

instance HaapMonad m => HasPlugin (StateDB st) (StateT (StateDB st)) m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT (StateT (StateDB st)) m (t2 m)) => HasPlugin (StateDB st) (ComposeT (StateT (StateDB st)) t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m

