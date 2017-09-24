{-# LANGUAGE EmptyDataDecls, TypeFamilies, InstanceSigs #-}

module HAAP.DB.State where

import HAAP.Core
import HAAP.DB
import HAAP.Lens

import Control.Monad
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState(..))
import qualified Control.Monad.State as State

data StateDB st

data StateDBDB st = StateDBDB st
data StateDBArgs st = StateDBArgs st

data StateDBQuery st a = StateDBQuery (st -> a)
data StateDBUpdate st a = StateDBUpdate (st -> (a,st))

instance HaapDB (StateDB st) where
    type DB (StateDB st) = StateDBDB st
    type DBArgs (StateDB st) = StateDBArgs st
    type DBQuery (StateDB st) a = StateDBQuery st a
    type DBUpdate (StateDB st) a = StateDBUpdate st a

    useDB getDB m = do
        args <- Reader.ask
        let StateDBArgs db = getDB args
        haapDBLens'' (constLens'' $ StateDBDB db) m
        
    queryDB (StateDBQuery q) = do
        StateDBDB st <- getDB
        return $ q st
        
    updateDB (StateDBUpdate u) = do
        StateDBDB st <- getDB
        let (x,st') = u st
        putDB $ StateDBDB st'
        return x
    
data DBLens db st = DBLens
    { dbGet :: DBQuery db st
    , dbPut :: st -> DBUpdate db ()
    }

lensDB :: (HaapMonad m,HaapDB db) => DBLens db st -> Haap p args (StateDBDB st) m a -> Haap p args (DB db) m a
lensDB l m = do
    let to = liftM StateDBDB $ queryDB $ dbGet l
    let from (StateDBDB x) = updateDB $ dbPut l x
    mapHaapDB to from m



