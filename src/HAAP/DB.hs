{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}

module HAAP.DB where

import HAAP.Core

class HaapDB db where
    type DB db = r | r -> db
    type DBArgs db = r | r -> db
    type DBQuery db a = r | r -> db a
    type DBUpdate db a = r | r -> db a
    
    useDB :: HaapMonad m => (args -> DBArgs db) -> Haap p args (DB db) m a -> Haap p args () m a
    
    queryDB :: HaapMonad m => DBQuery db a -> Haap p args (DB db) m a
    updateDB :: HaapMonad m => DBUpdate db a -> Haap p args (DB db) m a
    