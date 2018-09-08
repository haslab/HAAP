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
import Control.Exception.Safe
import Control.Monad

import Data.DList as DList
import qualified Data.Text as T

import GHC.Stack
import Data.Text.Prettyprint.Doc.Render.Text

logEvent :: (MonadIO m,HaapStack t m,HasCallStack) => T.Text -> Haap t m ()
logEvent msg = do
    liftHaap $ liftStack $ liftIO $ putDoc (text msg)
    Writer.tell $ DList.singleton $ HaapEvent callStack msg
    
logError :: (MonadIO m,HaapStack t m,HasCallStack) => SomeException -> Haap t m ()
logError err = logEvent $ prettyText err

printLog :: HaapLog -> IO ()
printLog l = forM_ l $ \e -> do
    putDoc $ pretty e
    putStrLn ""