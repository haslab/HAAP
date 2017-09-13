{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}

module HAAP.DB where

import HAAP.Core

class HaapDB db where
    type DB db = r | r -> db
    type DBArgs db = r | r -> db
    type DBQuery db a = r | r -> db a
    type DBUpdate db a = r | r -> db a
    
    useDB :: (args -> DBArgs db) -> Haap p args (DB db) a -> Haap p args () a
    
    queryDB :: DBQuery db a -> Haap p args (DB db) a
    updateDB :: DBUpdate db a -> Haap p args (DB db) a
    