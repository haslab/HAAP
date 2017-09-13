module HAAP.Log where

import HAAP.Core
import HAAP.Pretty

import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer as Writer

import Data.DList as DList

import GHC.Stack

logEvent :: HasCallStack => String -> Haap p args db ()
logEvent msg = do
    Writer.tell $ DList.singleton $ HaapEvent callStack msg
    
logError :: HasCallStack => HaapException -> Haap p args db ()
logError err = logEvent $ pretty err