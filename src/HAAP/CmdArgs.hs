{-
HAAP: Haskell Automated Assessment Platform

This module provides the @CmdArgs@ plugin (<https://hackage.haskell.org/package/cmdargs>) for command-line argument support.
-}

{-# LANGUAGE DeriveDataTypeable, TypeOperators, FlexibleInstances, UndecidableInstances, FlexibleContexts, TypeFamilies, MultiParamTypeClasses, ConstraintKinds #-}

module HAAP.CmdArgs where

import HAAP.Core
import HAAP.Plugin
import HAAP.IO

import System.Environment
import System.Console.CmdArgs hiding (CmdArgs,def,Default(..))

import Data.Default
import Data.Proxy
import Data.Typeable

import Control.Monad.Reader as Reader
import Control.Monad.Catch
import Control.Monad.Trans.Compose
import Control.Monad.Morph

data CmdArgs args = CmdArgs args
  deriving (Data,Typeable)

type HasCmdArgs args t m = HasPlugin (CmdArgs args) t m

instance (Data args) => HaapPlugin (CmdArgs args) where
    type PluginI (CmdArgs args) = CmdArgs args
    type PluginO (CmdArgs args) = ()
    type PluginT (CmdArgs args) = ReaderT (CmdArgs args)
    type PluginK (CmdArgs args) t m = (MonadIO m,Data args)

    usePlugin getArgs m = do
        CmdArgs argsDef <- getArgs
        args <- runBaseIO $ cmdArgs argsDef
        x <- mapHaapMonad (flip Reader.runReaderT (CmdArgs args) . getComposeT) m
        return (x,())

useCmdArgs :: (HaapStack t m,PluginK (CmdArgs args) t m) => args -> Haap (PluginT (CmdArgs args) :..: t) m a -> Haap t m a
useCmdArgs argsDef = usePlugin_ (return $ CmdArgs argsDef)

instance (MonadCatch m) => HasPlugin (CmdArgs args) (ReaderT (CmdArgs args)) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin (CmdArgs args) (ComposeT (ReaderT (CmdArgs args)) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m

readCmdArgs :: HasPlugin (CmdArgs args) t m => Haap t m args
readCmdArgs = do
    CmdArgs args <- liftHaap $ liftPluginProxy (Proxy::Proxy (CmdArgs args)) $ Reader.ask
    return args
