{-
HAAP: Haskell Automated Assessment Platform

This module provides a generic interface for HAAP sources, that are typically version-control repositories.
-}

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, TypeFamilyDependencies, RankNTypes, DoAndIfThenElse #-}

module HAAP.Sources where

import HAAP.Core
import HAAP.Lens
import HAAP.IO
import HAAP.Utils
import HAAP.Template
import HAAP.Log
import HAAP.Plugin
import HAAP.Shelly

import Data.Traversable
import Data.Default
import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Monad
import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class

import System.FilePath
import System.Directory
import System.FilePath.Find as FilePath

import Shelly (Sh(..))
import qualified Shelly as Sh

class (HaapPlugin s,Ord (SourceInfo s)) => HaapSource s where
    type Source s = r | r -> s
    type SourceInfo s = r | r -> s
    getSource :: (HasPlugin s t m,PluginK s t m) => Source s -> Haap t m ()
    putSource :: (HasPlugin s t m,PluginK s t m) => [FilePath] -> Source s -> Haap t m ()
    getSourceInfo  :: (HasPlugin s t m,PluginK s t m) => Source s -> Haap t m (SourceInfo s)
    
    sourcePath :: Source s -> FilePath
    sourcePath s = ""

-- | pushes project files to the group's local copy of the source; returns the files to be commited to version control system
populateGroupSource :: (MonadIO m,HasPlugin s t m,HaapSource s) => HaapContext -> Bool -> Group -> Source s -> Haap t m [FilePath]
populateGroupSource ctx overwriteStudentFiles g s = do
    logEvent "populating source"
    ppath <- getProjectPath
    let spath = sourcePath s
    files <- getProjectTaskFiles
    remotefiles <- runBaseSh $ forM files $ \file -> do
        --Sh.liftIO $ putStrLn $ "populating file " ++ show file
        let copy = haapFileType file == HaapLibraryFile || haapFileType file == HaapOracleFile || overwriteStudentFiles
        let localfile = haapLocalFile file
        let remotefile = applyTemplate (makeTemplate $ haapRemoteFile file) ctx
        let shCopy ctx from to = do
            docopy <- case haapFileType file of
                HaapTemplateFile -> do
                    isto <- shDoesFileExist to
                    return $ if isto then overwriteStudentFiles else True
                otherwise -> return True
            case haapFileType file of
                HaapBinaryFile -> when docopy $ shCp from to
                otherwise -> when docopy $ shLoadApplyAndCopyTemplate ctx from to
        shRecursive (shCopy ctx) (ppath </> localfile) (spath </> remotefile)
        case haapFileType file of
            HaapOracleFile -> return []
            otherwise -> return [remotefile]
    return $ concat remotefiles

listGroupSourceFiles :: (MonadIO m,HasPlugin s t m,HaapSource s) => HaapContext -> Bool -> Group -> Source s -> Haap t m [FilePath]
listGroupSourceFiles ctx ignoreLibrary g s = do
    let spath = sourcePath s
    hfiles <- getProjectTaskFiles
    let isIgnored HaapLibraryFile = ignoreLibrary
        isIgnored HaapOracleFile = True
        isIgnored HaapTemplateFile = False
        isIgnored HaapBinaryFile = False
    let mkIgnore f = applyTemplate (makeTemplate $ haapRemoteFile f) ctx
    ignorefiles <- runBaseIO' $ mapM (canonicalizePath . (spath </>) . mkIgnore) $ filter (isIgnored . haapFileType) hfiles
--    runIO $ putStrLn $ "listing " ++ show ignorefiles
    let notIgnored :: FindClause Bool
        notIgnored = do
            cp <- canonicalPath
            return $ not $ any (equalFilePath cp) ignorefiles
    hsfiles <- runBaseIO' $ FilePath.find (return True) (extension ==? ".hs" &&? notIgnored) spath
--    runIO $ putStrLn $ "hsfiles " ++ show hsfiles
    
    --let listRec :: FilePath -> Sh [FilePath]
    --    listRec p = do
    --        isignore <- anyM (equalPathSh p) ignorefiles
    --        if isignore then return []
    --        else do
    --            isdir <- shDoesDirectoryExist p
    --            if isdir then do
    --                xs <- shLs p
    --                ys <- mapM (listRec . (p </>)) xs
    --                return (concat ys)
    --            else return [p]
    --xs <- runSh $ do
    --    shCd (sourcePath s)
    --    listRec "."
    return $ map (makeRelative spath) hsfiles

    

