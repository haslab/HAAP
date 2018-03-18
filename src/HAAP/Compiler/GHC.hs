{-# LANGUAGE EmptyDataDecls, TypeOperators, GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module HAAP.Compiler.GHC where
    
import HAAP.IO
import HAAP.Core
import HAAP.Shelly
import HAAP.Plugin

import Data.Default

import Shelly (Sh(..))
import qualified Shelly as Sh

import Control.Monad.Reader as Reader
import Control.Monad.Catch

import Data.Proxy

data GHC

data GHCArgs = GHCArgs
    { ghcSafe :: Bool -- compile with the -XSafe extension
    , ghcHpc :: Bool -- compile for hpc
    , ghcArgs :: [String] -- additional flags
    , ghcRTS :: Bool
    , ghcIO :: IOArgs 
    }

instance Default GHCArgs where
    def = GHCArgs True False [] False def

instance HaapPlugin GHC where
    type PluginI GHC = GHCArgs
    type PluginO GHC = ()
    type PluginT GHC = ReaderT GHCArgs
    type PluginK GHC t m = (MonadIO m)

    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . unComposeT) m
        return (x,())

instance HaapMonad m => HasPlugin GHC (ReaderT GHCArgs) m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT (ReaderT GHCArgs) m (t2 m)) => HasPlugin GHC (ComposeT (ReaderT GHCArgs) t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m

useShGhc :: (HaapStack t Sh) => GHCArgs -> [FilePath] -> Haap t Sh IOResult
useShGhc args ins = usePlugin_ (return args) (shGhc ins)

shGhc :: (HasPlugin GHC t Sh) => [FilePath] -> Haap t Sh IOResult
shGhc ins = do
    args <- liftHaap $ liftPluginProxy (Proxy::Proxy GHC) Reader.ask
    liftSh $ shGhcWith args ins

shGhcWith :: GHCArgs -> [FilePath] -> Sh IOResult
shGhcWith ghc ins = shCommandWith (ghcIO ghc) "ghc" $ addArgs (ghcArgs ghc) $ addRTS (ghcRTS ghc) $ addHpc (ghcHpc ghc) $ addSafe (ghcSafe ghc) ins
    where
    addSafe True cmds = "-XSafe" : cmds
    addSafe False cmds = cmds
    addHpc True  cmds = "-fhpc" : cmds
    addHpc False cmds = cmds
    addRTS True  cmds = "-rtsopts" : cmds
    addRTS False cmds = cmds
    addArgs xs ys = ys ++ xs