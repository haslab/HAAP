{-
HAAP: Haskell Automated Assessment Platform

This module provides a generic interface for HAAP plugins.
HAAP follows a 'configuration-as-an-application' architecture, common in Haskell web frameworks such as Happstack.
The plugins abstraction is defined to be reusable and composable, and each feature of HAAP is provided via a different plugin.
This is facilitated by the ability to load plugins into a stack (|usePlugin|) and use plugins at any level of the stack (|liftPlugin|).
-}

{-# LANGUAGE StandaloneDeriving, UndecidableSuperClasses, RankNTypes, TypeOperators, UndecidableInstances, FlexibleInstances, FunctionalDependencies, TypeFamilyDependencies, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, ConstraintKinds, GeneralizedNewtypeDeriving, ScopedTypeVariables, InstanceSigs, DefaultSignatures #-}
module HAAP.Plugin
    ( module HAAP.Plugin
    , module Control.Monad.Morph
    , module Control.Monad.Trans.Compose
    , module Control.Monad.Trans.Control
    ) where

import Control.Monad.Base
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State 
import Control.Monad.RWS
--import Control.Monad.Catch
import Control.Monad.Morph hiding (embed)
import Control.Exception.Safe

import GHC.Exts

import Data.Proxy

import HAAP.Core

class HaapPlugin p where
    type PluginI p = (r :: *) | r -> p
    type PluginO p :: *
    type PluginT p = (t :: (* -> *) -> * -> *) | t -> p
    type PluginK p (t :: (* -> *) -> * -> *) (m :: * -> *) :: Constraint
    
    usePlugin :: (HaapStack t m,PluginK p t m) => Haap t m (PluginI p) -> Haap (PluginT p :..: t) m a -> Haap t m (a,PluginO p)
    usePlugin_ :: (HaapStack t m,PluginK p t m) => Haap t m (PluginI p) -> Haap (PluginT p :..: t) m a -> Haap t m a
    usePlugin_ minp m = liftM fst $ usePlugin minp m

class (HaapStack t m) => HasPlugin p (t :: (* -> *) -> * -> *) m where
    liftPlugin :: PluginT p m a -> t m a
    liftPlugin = liftPluginProxy Proxy
    liftPluginProxy :: Proxy p -> PluginT p m a -> t m a
    liftPluginProxy _ = liftPlugin

instance {-# OVERLAPPABLE #-} (MFunctor t1,HaapStack t1 (t2 m),HasPlugin p t2 m) => HasPlugin p (t1 :..: t2) m where
    liftPluginProxy proxy m = ComposeT $ lift $ liftPluginProxy proxy m

instance (MFunctor' (Haap t) m,HasPlugin p t m) => HasPlugin p (Haap t) m where
    liftPluginProxy proxy m = liftHaap $ liftPluginProxy proxy m

instance (Monad (t2 m),MFunctor' t1 (t2 m),MFunctor' t2 m) => MFunctor' (t1 :..: t2) m where
    hoist' f = ComposeT . hoist' (hoist' f) . getComposeT

instance (Monad (t m),MFunctor' t m) => MFunctor' (Haap t) m where
    hoist' f (Haap m) = Haap $ hoist' f m

-- * Plugin classes

type HaapPureStack t m n = (MonadTransControl' t m n,HaapStack t m,HaapStack t n)
type HaapPureBase t m b = (MonadTrans t,HaapPureStack t m m,MonadBaseControl b m)
  
deriving instance MonadBase b (t1 (t2 m)) => MonadBase b (ComposeT t1 t2 m)
deriving instance MonadBaseControl b (t1 (t2 m)) => MonadBaseControl b (ComposeT t1 t2 m)
    
instance (Monad (t m),MonadTrans t, MonadBase b m) => MonadBase b (Haap t m) where
    liftBase b = Haap $ liftBaseDefault b
    
instance (Monad (t m),MonadTrans t,MonadBaseControl b m,MonadTransControl' t m m) => MonadBaseControl b (Haap t m) where
    type StM (Haap t m) a = ComposeSt' (RWST Project HaapLog () :..: t) m a
    liftBaseWith f = Haap $ defaultLiftBaseWith' $ \runInBase -> f (runInBase . unHaap)
    restoreM st = Haap $ defaultRestoreM' st
    
type ComposeSt' t m a = StM m (StT' t a)
type RunInBaseDefault' t m b = forall a. t m a -> b (ComposeSt' t m a)

defaultLiftBaseWith' :: (MonadTransControl' t m m, MonadBaseControl b m) => (RunInBaseDefault' t m b -> b a) -> t m a
defaultLiftBaseWith' = \f -> liftWith' $ \run -> liftBaseWith $ \runInBase -> f $ runInBase . run
    
defaultRestoreM' :: (MonadTransControl' t m m, MonadBaseControl b m) => ComposeSt' t m a -> t m a
defaultRestoreM' = restoreT' . restoreM
    
infixr 9 :..:
type (:..:) = ComposeT

class (Monad m,Monad (t m)) => MonadTransRestore' t m where
    type StT' t a :: *
    restoreT' :: m (StT' t a) -> t m a

class (MonadTransRestore' t m) => MonadTransControl' t m n where
    liftWith' :: (Run' n t -> m a) -> t m a

type Run' n t = forall b. t n b -> n (StT' t b)

instance (Monad m,MonadTransControl IdentityT) => MonadTransRestore' IdentityT m where
    type StT' IdentityT a = StT IdentityT a
    restoreT' = restoreT
instance (Monad m,Monad n,MonadTransControl IdentityT) => MonadTransControl' IdentityT m n where
    liftWith' = liftWith

instance (Monad m,MonadTransControl (ReaderT r)) => MonadTransRestore' (ReaderT r) m where
    type StT' (ReaderT r) a = StT (ReaderT r) a
    restoreT' = restoreT
instance (Monad m,Monad n,MonadTransControl (ReaderT r)) => MonadTransControl' (ReaderT r) m n where
    liftWith' = liftWith

instance (Monad m,MonadTransControl (StateT r)) => MonadTransRestore' (StateT r) m where
    type StT' (StateT r) a = StT (StateT r) a
    restoreT' = restoreT
instance (Monad m,Monad n,MonadTransControl (StateT r)) => MonadTransControl' (StateT r) m n where
    liftWith' = liftWith

instance (Monad m,Monoid w,MonadTransControl (WriterT w)) => MonadTransRestore' (WriterT w) m where
    type StT' (WriterT w) a = StT (WriterT w) a
    restoreT' = restoreT
instance (Monad m,Monad n,Monoid w,MonadTransControl (WriterT w)) => MonadTransControl' (WriterT w) m n where
    liftWith' = liftWith

instance (Monad m,Monoid w,MonadTransControl (RWST r w s)) => MonadTransRestore' (RWST r w s) m where
    type StT' (RWST r w s) a = StT (RWST r w s) a
    restoreT' = restoreT
instance (Monad m,Monad n,Monoid w,MonadTransControl (RWST r w s)) => MonadTransControl' (RWST r w s) m n where
    liftWith' = liftWith

instance (MonadTransRestore' t1 (t2 m),MonadTransRestore' t2 m) => MonadTransRestore' (t1 :..: t2) m where
    type StT' (t1 :..: t2) a = StT' t2 (StT' t1 a)
    restoreT' = ComposeT . restoreT' . restoreT'
instance (MonadTransControl' t1 (t2 m) (t2 n),MonadTransControl' t2 m n) => MonadTransControl' (t1 :..: t2) m n where
    liftWith' f = ComposeT $ liftWith' $ \run -> liftWith' $ \run' -> f $ run' . run . getComposeT


instance (Monad (t m),MonadTransRestore' t m) => MonadTransRestore' (Haap t) m where
    type StT' (Haap t) a = StT' (RWST Project HaapLog () :..: t) a
    restoreT' = Haap . restoreT'
instance (Monad (t m),Monad (t n),MonadTransControl' t m n) => MonadTransControl' (Haap t) m n where
    liftWith' f = Haap $ liftWith' $ \run -> f $ run . unHaap


