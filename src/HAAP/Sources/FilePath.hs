{-
HAAP: Haskell Automated Assessment Platform

This module provides the @FilePathSource@ plugin that provides the simplest instance of a @HaapSource@ as a filepath.
-}

{-# LANGUAGE TypeOperators, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, EmptyDataDecls #-}

module HAAP.Sources.FilePath where

import HAAP.Core
import HAAP.IO
import HAAP.Sources
import HAAP.Plugin
import HAAP.Shelly
import HAAP.Log

import Data.Default
import Data.List.Split
import Data.List
import qualified Data.Text as Text
import Data.SafeCopy
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Maybe
import Data.Proxy

import Shelly (Sh)

import Control.Monad.Reader as Reader
import Control.Monad.Trans.Compose
--import Control.Monad.Except

import Text.Read hiding (lift)

import System.FilePath
import System.Directory
import System.Locale.Read 

import qualified Shelly as Sh

import Safe

data FilePathSource = FilePathSource FilePath
$(deriveSafeCopy 0 'base ''FilePathSource)

data FilePathSourceInfo = FilePathSourceInfo
  deriving (Show,Eq,Ord)
$(deriveSafeCopy 0 'base ''FilePathSourceInfo)

data FilePathSourceArgs = FilePathSourceArgs
  deriving Show
$(deriveSafeCopy 0 'base ''FilePathSourceArgs)

instance HaapPlugin FilePathSource where
    type PluginI FilePathSource = FilePathSourceArgs
    type PluginT FilePathSource = ReaderT FilePathSourceArgs
    type PluginO FilePathSource = ()
    type PluginK FilePathSource t m = (MonadIO m)
    
    usePlugin getArgs m = do
        args <- getArgs
        let go (ComposeT m) = do
            Reader.runReaderT m args
        x <- mapHaapMonad go m
        return (x,())

useFilePathSource :: (HaapStack t m,PluginK FilePathSource t m) => PluginI FilePathSource -> Haap (PluginT FilePathSource :..: t) m a -> Haap t m a
useFilePathSource svnargs m = usePlugin_ (return svnargs) m

instance HaapSource FilePathSource where
    type Source FilePathSource = FilePathSource
    type SourceInfo FilePathSource = FilePathSourceInfo
    getSource s = return ()
    putSource files (FilePathSource s) = runBaseSh $ forM_ files $ \file -> shCp file s
    getSourceInfo s = return FilePathSourceInfo
    
    sourcePath (FilePathSource p) = p

defaultFilePathSourceArgs = FilePathSourceArgs

instance Default FilePathSourceArgs where
    def = defaultFilePathSourceArgs
    
--instance HaapMonad m => HaapStack (ReaderT FilePathSourceArgs) m where
--    liftStack = lift

instance (HaapMonad m) => HasPlugin FilePathSource (ReaderT FilePathSourceArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin FilePathSource (ComposeT (ReaderT FilePathSourceArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m