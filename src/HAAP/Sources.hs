{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, TypeFamilyDependencies, RankNTypes, DoAndIfThenElse #-}

module HAAP.Sources where

import HAAP.Core
import HAAP.Lens
import HAAP.IO
import HAAP.Utils
import HAAP.Template
import HAAP.Log

import Data.Traversable
import Data.Default
import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Monad
import qualified Control.Monad.Reader as Reader

import System.FilePath
import System.Directory
import System.FilePath.Find as FilePath

import Shelly (Sh(..))
import qualified Shelly as Sh

class Ord (SourceInfo s) => HaapSource s where
    type Source s = r | r -> s
    type SourceInfo s = r | r -> s
    type SourceArgs s = r | r -> s
    getSourceWith :: HaapMonad m => (args -> SourceArgs s) -> Source s -> Haap p args db m ()
    putSourceWith :: HaapMonad m => (args -> SourceArgs s) -> [FilePath] -> Source s -> Haap p args db m ()
    getSourceInfoWith  :: HaapMonad m => (args -> SourceArgs s) -> Source s -> Haap p args db m (SourceInfo s)
    
    sourcePath :: Source s -> FilePath
    sourcePath s = ""

-- pulls the latest source (a.k.a. git pull)
getSource :: (HaapMonad m,HaapSource s) => Source s -> Haap p (SourceArgs s) db m ()
getSource = getSourceWith id

-- adds files to the source and pushes a new source (a.k.a git push)
putSource :: (HaapMonad m,HaapSource s) => [FilePath] -> Source s -> Haap p (SourceArgs s) db m ()
putSource = putSourceWith id

-- gets detailed source information
getSourceInfo :: (HaapMonad m,HaapSource s) => Source s -> Haap p (SourceArgs s) db m (SourceInfo s)
getSourceInfo = getSourceInfoWith id

-- | pushes project files to the group's local copy of the source; returns the files to be commited to version control system
populateGroupSourceWith :: (HaapMonad m,HaapSource s) => (args -> SourceArgs s) -> HaapContext -> Bool -> Group -> Source s -> Haap p args db m [FilePath]
populateGroupSourceWith getArgs ctx overwriteTemplateFiles g s = do
    logEvent "populating source"
    sargs <- Reader.reader getArgs
    ppath <- getProjectPath
    let spath = sourcePath s
    files <- getProjectTaskFiles
    remotefiles <- runSh $ forM files $ \file -> do
        --Sh.liftIO $ putStrLn $ "populating file " ++ show file
        let copy = haapFileType file == HaapLibraryFile || haapFileType file == HaapOracleFile || overwriteTemplateFiles
        let localfile = haapLocalFile file
        let remotefile = applyTemplate (makeTemplate $ haapRemoteFile file) ctx
        if copy
            then do
--                Sh.liftIO $ putStrLn $ "from " ++ show  (ppath </> localfile)
--                Sh.liftIO $ putStrLn $ "to " ++ show (spath </> remotefile)
                shRecursive (shLoadApplyAndCopyTemplate ctx) (ppath </> localfile) (spath </> remotefile)
                case haapFileType file of
                    HaapOracleFile -> return []
                    otherwise -> return [remotefile]
            else return []
    return $ concat remotefiles

listGroupSourceFiles :: (HaapMonad m,HaapSource s) => HaapContext -> Bool -> Group -> Source s -> Haap p (SourceArgs s) db m [FilePath]
listGroupSourceFiles ctx ignoreLibrary g s = do
    let spath = sourcePath s
    hfiles <- getProjectTaskFiles
    let isIgnored HaapLibraryFile = ignoreLibrary
        isIgnored HaapOracleFile = True
        isIgnored HaapTemplateFile = False
    let mkIgnore f = applyTemplate (makeTemplate $ haapRemoteFile f) ctx
    ignorefiles <- runIO $ mapM (canonicalizePath . (spath </>) . mkIgnore) $ filter (isIgnored . haapFileType) hfiles
    let notIgnored :: FindClause Bool
        notIgnored = do
            cp <- canonicalPath
            return $ not $ any (equalFilePath cp) ignorefiles
    hsfiles <- runIO $ FilePath.find (return True) (extension ==? ".hs" &&? notIgnored) spath
    
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

    

