{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}

module HAAP.Sources.SVN where

import HAAP.Core
import HAAP.IO
import HAAP.Sources

import Data.Default
import Data.List.Split
import Data.List
import qualified Data.Text as Text

import qualified Control.Monad.Reader as Reader
import Control.Monad.Except

import Text.Read

import System.FilePath
import System.Directory

import Safe

data SVN

data SVNSource = SVNSource
    { svnUser :: String
    , svnPass :: String
    , svnPath :: FilePath -- path on disk
    , svnRepository :: FilePath -- repository url
    }
  deriving Show

data SVNSourceInfo = SVNSourceInfo
    { svnRevision :: Int
    , svnAuthor  :: String
    , svnDate     :: String
    }
  deriving Show

instance Eq SVNSourceInfo where
    x == y = (svnRevision x) == (svnRevision y)

instance Ord SVNSourceInfo where
    compare x y = compare (svnRevision x) (svnRevision y)

data SVNSourceArgs = SVNSourceArgs
    { svnCommitMessage :: String
    }
  deriving Show

instance HaapSource SVN where
    type Source SVN = SVNSource
    type SourceInfo SVN = SVNSourceInfo
    type SourceArgs SVN = SVNSourceArgs
    getSourceWith = getSVNSourceWith
    putSourceWith = putSVNSourceWith
    getSourceInfoWith = getSVNSourceInfoWith

defaultSVNSourceArgs = SVNSourceArgs "system commit"

instance Default SVNSourceArgs where
    def = defaultSVNSourceArgs
    
    
getSVNSourceWith :: (args -> SVNSourceArgs) -> SVNSource -> Haap p args db ()
getSVNSourceWith getArgs s = do
    let user = svnUser s
    let pass = svnPass s
    let path = svnPath s
    let repo = svnRepository s
    let (dir,name) = splitFileName path
    exists <- orLogDefault False $ runIO $ doesDirectoryExist path
    runSh $ do
        if exists
            then do
                shCd path
                shCommand "svn" ["cleanup"]
                shCommand "svn" ["update","--non-interactive","--username",user,"--password",pass]
            else do
                shCd dir
                shCommand "svn" ["checkout",repo,"--non-interactive",name,"--username",user,"--password",pass]
    return ()

getSVNSourceInfoWith :: (args -> SVNSourceArgs) -> SVNSource -> Haap p args db SVNSourceInfo
getSVNSourceInfoWith getArgs s = do
    let path = svnPath s
    info <- runSh $ do
        shCd path
        shCommand "svn" ["info","--non-interactive"]
    rev <- parseInfo $ resStdout info
    logRev <- runSh $ do
        shCd path
        shCommand "svn" ["log","-r",show rev]
    (author,date) <- parseLogRev $ resStdout logRev
    return $ SVNSourceInfo rev author date
  where
    parseInfo txt = case dropWhile (not . isPrefixOf "Revision:") (lines $ Text.unpack txt) of
        (x:xs) -> case readMaybe (drop 10 x) :: Maybe Int of
            Just rev -> return rev
            Nothing -> throwError $ HaapException $ "failed to parse svn info revision for " ++ show s
        [] -> throwError $ HaapException $ "failed to parse svn info revision for " ++ show s
    parseLogRev txt = case headMay $ tailDef [] (lines $ Text.unpack txt) of
        Just str -> case splitOn "|" str of
            [_,author,date,_] -> return (author,date)
            otherwise -> throwError $ HaapException $ "failed to parse svn revision log for " ++ show s
        Nothing -> throwError $ HaapException $ "failed to parse svn revision log for " ++ show s

putSVNSourceWith :: (args -> SVNSourceArgs) -> SVNSource -> Haap p args db ()
putSVNSourceWith getArgs s = do
    args <- Reader.reader getArgs
    let user = svnUser s
    let pass = svnPass s
    let path = svnPath s
    let msg = svnCommitMessage args
    runSh $ do
        shCd path
        shCommand "svn" ["commit","-m",show msg,"--non-interactive","--username",user,"--password",pass]
    return ()






