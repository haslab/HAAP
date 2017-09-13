{-# LANGUAGE TypeFamilies #-}

module HAAP.Sources.SVN where

import HAAP.Sources

import Data.Default
import Data.DateTime

data SVN

data SVNSource = SVNSource
    { svnUser :: String
    , svnPass :: String
    }

data SVNSourceInfo = SVNSourceInfo
    { svnRevision :: Int
    , svnData     :: DateTime
    }

data SVNSourceArgs = SVNSourceArgs
    { updateSVN :: Bool -- when getting sources, update to their latest version
    , overwriteSVN :: Bool -- when putting sources, overwrite existing files
    }

instance HaapSource SVN where
    type Source SVN = SVNSource
    type SourceInfo SVN = SVNSourceInfo
    type SourceArgs SVN = SVNSourceArgs
    getSourceWith = undefined
    putSourceWith = undefined
    defaultSourceArgs = defaultSVNSourceArgs

defaultSVNSourceArgs = SVNSourceArgs True False

instance Default SVNSourceArgs where
    def = defaultSVNSourceArgs

