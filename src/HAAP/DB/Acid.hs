{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleContexts, RankNTypes, GADTs, TypeFamilies, InstanceSigs, ScopedTypeVariables, TemplateHaskell #-}

module HAAP.DB.Acid where

import HAAP.Core
import HAAP.DB
import HAAP.DB.State
import HAAP.Lens
import HAAP.IO

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer as Writer
import Control.Monad.State (MonadState(..))
import qualified Control.Monad.State as State

import Data.Acid
import Data.Acid.Advanced
import Data.SafeCopy
import Data.Typeable

import System.FilePath

data AcidDB st

data AcidDBArgs st = AcidDBArgs
    { acidDBFile :: FilePath -- relative filepath
    , acidDBInit :: st -- initial database
    }

data AcidDBQuery st a where
    AcidDBQuery :: (QueryEvent ev,EventState ev ~ st,EventResult ev ~ a) => ev -> AcidDBQuery st a

data AcidDBUpdate st a where
    AcidDBUpdate :: (UpdateEvent ev,EventState ev ~ st,EventResult ev ~ a) => ev -> AcidDBUpdate st a

instance IsAcidic st => HaapDB (AcidDB st) where
    type DB (AcidDB st) = AcidState st
    type DBArgs (AcidDB st) = AcidDBArgs st
    type DBQuery (AcidDB st) a = AcidDBQuery st a
    type DBUpdate (AcidDB st) a = AcidDBUpdate st a
    
    useDB getArgs m = do
        path <- getProjectPath
        args <- liftM getArgs Reader.ask
        acid <- runIO $ openLocalStateFrom (path </> acidDBFile args) (acidDBInit args)
        x <- haapDBLens'' (constLens'' acid) m
        runIO $ createArchive acid
        runIO $ closeAcidState acid
        return x
        
    queryDB (AcidDBQuery q) = do
        acid <- getDB
        runIO $ query acid q
        
    updateDB (AcidDBUpdate u) = do
        acid <- getDB
        runIO $ update acid u


    
    

--    acidEvents = [QueryEvent $ \(AcidDBQuery q) -> q]

--class AcidTrans db st where
--    acidGet :: DBQuery db st
--    acidPut :: st -> DBUpdate db ()
--
--data AcidT db st
--
--type AcidTQuery db st a = Either (AcidDBQuery db a) (AcidDBQuery st a)
--data AcidTUpdate db st a where
--    AcidTUpdateDb :: AcidDBUpdate db a -> 
--    AcidTUpdateSt :: ev -> 
--    
--acidTUpdateSt :: Update db ()
--acidTUpdateSt 
--
--instance HaapDB (AcidT db st) where
--    
--    type DB (AcidT db st) = AcidState db
--    
--    queryDB :: AcidTQuery db st a -> Haap p args (AcidT db st) a
--    queryDB (Left q) = queryDB q
--    queryDB (Right q) = queryDB q
--
--instance (IsAcidic db,IsAcidib st) => IsAcidic (AcidT db st) where
----    acidEvents = acidEvents ++ acidEvents








