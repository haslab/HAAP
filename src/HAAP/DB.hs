{-# LANGUAGE ConstraintKinds, TypeFamilies, TypeFamilyDependencies #-}

module HAAP.DB where

import HAAP.Core
import HAAP.Plugin

class HaapPlugin db => HaapDB db where
    type DBQuery db a = r | r -> db a
    type DBUpdate db a = r | r -> db a
    
    queryDB :: (HasPlugin db t m,PluginK db t m) => DBQuery db a -> Haap t m a
    updateDB :: (HasPlugin db t m,PluginK db t m) => DBUpdate db a -> Haap t m a

type HasDB db t m = (HaapDB db,HasPlugin db t m)

data DBLens db st = DBLens
    { dbGet :: DBQuery db st
    , dbPut :: st -> DBUpdate db ()
    }
    