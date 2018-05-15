{-# LANGUAGE ForeignFunctionInterface #-}

module XtermArgs where

import qualified Data.Text as T 
import qualified Data.Text.Lazy as LT (Text)

import Data.Typeable (Typeable)
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int64)
import Data.Word (Word, Word64)
import Data.Maybe (fromJust)
import Data.Traversable (mapM)
import Control.Applicative ((<$>))

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Data.IORef
import Data.Char

import Paths_Xterm

getXtermFile :: FilePath -> IO FilePath
getXtermFile = getDataFileName 

data TerminalOpts = TerminalOpts
    { cursorBlink :: Bool
    , rows :: Int
    , cols :: Int
    , scrollback :: Int -- The number of rows to be persisted in terminal buffer for scrolling (default: 1000).
    , tabStopWidth :: Int -- The number of columns a tab stop should occupy (default: 8).
    }

defaultTerminalOpts = TerminalOpts True 30 30 1000 8