{-# LANGUAGE EmptyDataDecls, TypeFamilies, InstanceSigs #-}

module HAAP.DB.Binary where

import HAAP.Core
import HAAP.DB
import HAAP.Lens
import HAAP.IO

import Data.Binary

import Control.Monad
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState(..))
import qualified Control.Monad.State as State

import System.FilePath

data BinaryDB st

data BinaryDBDB st = BinaryDBDB st
data BinaryDBArgs st = BinaryDBArgs
    { binaryDBFile :: FilePath -- relative filepath
    , binaryDBInit :: st -- initial database
    }

data BinaryDBQuery st a = BinaryDBQuery (st -> a)
data BinaryDBUpdate st a = BinaryDBUpdate (st -> (a,st))

instance Binary st => HaapDB (BinaryDB st) where
    type DB (BinaryDB st) = BinaryDBDB st
    type DBArgs (BinaryDB st) = BinaryDBArgs st
    type DBQuery (BinaryDB st) a = BinaryDBQuery st a
    type DBUpdate (BinaryDB st) a = BinaryDBUpdate st a

    useDB getArgs m = do
        path <- getProjectPath
        args <- Reader.reader getArgs
        let file = path </> binaryDBFile args
        let get = liftM BinaryDBDB $ orDefault (\err -> return $ binaryDBInit args) (runIO $ decodeFile file)
        let put (BinaryDBDB st) = runIO $ encodeFile file st
        mapHaapDB get put m
        
    queryDB (BinaryDBQuery q) = do
        BinaryDBDB st <- getDB
        return $ q st
        
    updateDB (BinaryDBUpdate u) = do
        BinaryDBDB st <- getDB
        let (x,st') = u st
        putDB $ BinaryDBDB st'
        return x
    




