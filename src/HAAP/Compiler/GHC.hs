{-
HAAP: Haskell Automated Assessment Platform

This module provides the @GHC@ plugin runs the _ghc_ Haskell compiler.

-}


{-# LANGUAGE EmptyDataDecls, TypeOperators, OverloadedStrings, GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module HAAP.Compiler.GHC where
    
import HAAP.IO
import HAAP.Core
import HAAP.Shelly
import HAAP.Plugin
import HAAP.Pretty
import HAAP.Log

import Data.Default

import Shelly (Sh(..))
import qualified Shelly as Sh

import Control.Monad.Reader as Reader
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Compose
import Control.Monad.Morph

import System.FilePath

import Data.Proxy

data GHC

data GHCArgs = GHCArgs
    { ghcSafe :: Bool -- compile with the -XSafe extension
    , ghcHpc :: Bool -- compile for hpc
    , ghcArgs :: [String] -- additional flags
    , ghcRTS :: Bool
    , ghcIO :: IOArgs 
    , ghcMake :: Bool -- --make flag
    }

instance Default GHCArgs where
    def = GHCArgs True False [] False def False

instance HaapPlugin GHC where
    type PluginI GHC = GHCArgs
    type PluginO GHC = ()
    type PluginT GHC = ReaderT GHCArgs
    type PluginK GHC t m = (MonadIO m)

    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . getComposeT) m
        return (x,())

instance (HaapMonad m) => HasPlugin GHC (ReaderT GHCArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin GHC (ComposeT (ReaderT GHCArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m

useShGhc :: (HaapStack t Sh) => GHCArgs -> [FilePath] -> Haap t Sh IOResult
useShGhc args ins = usePlugin_ (return args) (shGhc ins)    

shGhc :: (HasPlugin GHC t Sh) => [FilePath] -> Haap t Sh IOResult
shGhc ins = do
    args <- liftPluginProxy (Proxy::Proxy GHC) Reader.ask
    liftSh $ shGhcWith args ins

shGhcWith :: GHCArgs -> [FilePath] -> Sh IOResult
shGhcWith ghc ins = shCommandWith (ghcIO ghc) "ghc" $ addArgs (ghcArgs ghc) $ addRTS (ghcRTS ghc) $ addHpc (ghcHpc ghc) $ addSafe (ghcSafe ghc) $ addMake ins
    where
    addSafe True cmds = "-XSafe" : cmds
    addSafe False cmds = cmds
    addHpc True  cmds = "-fhpc" : cmds
    addHpc False cmds = cmds
    addRTS True  cmds = "-rtsopts" : cmds
    addRTS False cmds = cmds
    addArgs xs ys = ys ++ xs
    addMake cmds = if ghcMake ghc then "--make":cmds else cmds
    
useAndRunGhcExec :: (MonadIO m,HaapStack t m) => GHCArgs -> FilePath -> (IOResult -> Haap (ReaderT GHCArgs :..: t) m a) -> Haap t m (a)
useAndRunGhcExec args ghcexec m = usePlugin_ (return args) $ runGhcExec ghcexec m

runGhcExec :: (MonadIO m,HasPlugin GHC t m) => FilePath -> (IOResult -> Haap t m a) -> Haap t m a
runGhcExec ghcexec m = do
    ghc <- liftHaap $ liftPluginProxy (Proxy::Proxy GHC) $ Reader.ask
    let (dir,exec) = splitFileName ghcexec
    let ghc' = ghc { ghcMake = True }
    logEvent $ "Compiling GHC executable " <> prettyText ghcexec
    ghcres <- orIOResult $ runBaseSh $ do
        shCd dir
        res <- shGhcWith ghc' [exec]
        return $! res
    m ghcres
    