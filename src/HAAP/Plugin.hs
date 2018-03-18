{-# LANGUAGE UndecidableSuperClasses, RankNTypes, TypeOperators, UndecidableInstances, FlexibleInstances, FunctionalDependencies, TypeFamilyDependencies, TypeFamilies, MultiParamTypeClasses, FlexibleContexts, ConstraintKinds, GeneralizedNewtypeDeriving #-}
module HAAP.Plugin where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State 
import Control.Monad.RWS
import Control.Monad.Catch
import Control.Monad.Morph

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

instance {-# OVERLAPPABLE #-} (HaapStack t1 (t2 m),HasPlugin p t2 m) => HasPlugin p (ComposeT t1 t2) m where
    liftPluginProxy proxy m = ComposeT $ liftStack $ liftPluginProxy proxy m

instance HasPlugin p t m => HasPlugin p (Haap t) m where
    liftPluginProxy proxy m = liftHaap $ liftPluginProxy proxy m

-- * Plugin classes

-- * Base plugins

-- | @MFunctor@ equivalent
class (HaapStack t m,HaapStack t n) => HaapPluginT t m n where
    hoistPluginT :: (forall a . m a -> n a) -> t m b -> t n b

instance {-# OVERLAPPABLE #-} (HaapMonad (t m),HaapMonad (t n),HaapMonad m,HaapMonad n,MonadTrans t,MFunctor t) => HaapPluginT t m n where
    hoistPluginT = hoist

-- * Pure plugins

-- | @MonadTransControl@ restore equivalent
class HaapStack t m => HaapPureRestoreT t m where
    type StPluginT t a :: *
    restorePluginT :: m (StPluginT t a) -> t m a

-- | @MonadTransControl@ lift equivalent
class (HaapPluginT t m n,HaapPureRestoreT t m,HaapPureRestoreT t n) => HaapPureLiftT t m n where
    liftWithPluginT :: (RunPluginT t n -> m a) -> t m a

type RunPluginT t n = forall b. t n b -> n (StPluginT t b)

type HaapPurePluginT t m = (HaapPureRestoreT (Haap t) m,HaapPureLiftT (Haap t) m m)

-- * Plugin composition

newtype ComposeT (t1 :: (* -> *) -> * -> *) (t2 :: (* -> *) -> * -> *) (m :: * -> *) (a :: *) = ComposeT { unComposeT :: t1 (t2 m) a }
  deriving (Applicative,Monad,MonadCatch,MonadThrow)

instance Functor (t1 (t2 m)) => Functor (ComposeT t1 t2 m) where
    fmap f (ComposeT m) = ComposeT $ fmap f m

infixr 9 :..:
type (:..:) = ComposeT

instance (HaapStack t1 (t2 m),HaapStack t2 m) => HaapStack (ComposeT t1 t2) m where
    liftStack m = ComposeT $ liftStack $ liftStack m

instance (HaapPluginT t1 (t2 m) (t2 n),HaapPluginT t2 m n) => HaapPluginT (ComposeT t1 t2) m n where
    hoistPluginT f (ComposeT m) = ComposeT $ hoistPluginT (hoistPluginT f) m

instance (Monad (t2 m),MonadReader r (t1 (t2 m))) => MonadReader r (ComposeT t1 t2 m) where
    ask = ComposeT ask
    local f (ComposeT m) = ComposeT $ local f m

instance (Monad (t2 m),MonadWriter w (t1 (t2 m))) => MonadWriter w (ComposeT t1 t2 m) where
    writer = ComposeT . writer
    listen = ComposeT . listen . unComposeT
    pass = ComposeT . pass . unComposeT

instance (HaapPureRestoreT t1 (t2 m),HaapPureRestoreT t2 m) => HaapPureRestoreT (ComposeT t1 t2) m where
    type StPluginT (ComposeT t1 t2) a = StPluginT t2 (StPluginT t1 a)
    restorePluginT = ComposeT . restorePluginT . restorePluginT

instance (HaapPureRestoreT t1 m,HaapPureRestoreT t1 n,HaapPureRestoreT t1 (t2 n),HaapPureLiftT t1 (t2 m) (t2 n),HaapPureLiftT t2 m n) => HaapPureLiftT (ComposeT t1 t2) m n where
    liftWithPluginT f = ComposeT $ liftWithPluginT $ \run -> liftWithPluginT $ \run' -> f $ run' . run . unComposeT

