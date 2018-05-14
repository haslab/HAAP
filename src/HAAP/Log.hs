{-
HAAP: Haskell Automated Assessment Platform

This module provides basic logging capabilities.
-}

module HAAP.Log where

import HAAP.Core
import HAAP.Pretty

import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer as Writer
import Control.Monad.IO.Class

import Data.DList as DList

import GHC.Stack

logEvent :: (MonadIO m,HaapStack t m,HasCallStack) => String -> Haap t m ()
logEvent msg = do
    liftHaap $ liftStack $ liftIO $ putStrLn msg
    Writer.tell $ DList.singleton $ HaapEvent callStack msg
    
logError :: (MonadIO m,HaapStack t m,HasCallStack) => HaapException -> Haap t m ()
logError err = logEvent $ pretty err