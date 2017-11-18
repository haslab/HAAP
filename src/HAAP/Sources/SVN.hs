{-# LANGUAGE TemplateHaskell, TypeFamilies, EmptyDataDecls #-}

module HAAP.Sources.SVN where

import HAAP.Core
import HAAP.IO
import HAAP.Sources

import Data.Default
import Data.List.Split
import Data.List
import qualified Data.Text as Text
import Data.SafeCopy
import Data.Time.Format
import Data.Time.LocalTime

import qualified Control.Monad.Reader as Reader
import Control.Monad.Except

import Text.Read

import System.FilePath
import System.Directory
import System.Locale.Read

import qualified Shelly as Sh

import Safe

data SVN

parseSVNDateDefault :: MonadIO m => String -> m ZonedTime
parseSVNDateDefault str = parseSVNDateWith defaultTimeLocale str

parseSVNDateCurrent :: MonadIO m => String -> m ZonedTime
parseSVNDateCurrent str = do
    locale <- liftIO $ getCurrentLocale
    parseSVNDateWith locale str

parseSVNDateWith :: Monad m => TimeLocale -> String -> m ZonedTime
parseSVNDateWith locale str = parseTimeM True locale format str
    where
    format = "%F %T %z (%a, %d %b %Y)"

data SVNSource = SVNSource
    { svnUser :: String
    , svnPass :: String
    , svnPath :: FilePath -- path on disk
    , svnRepository :: FilePath -- repository url
    }
$(deriveSafeCopy 0 'base ''SVNSource)

instance Show SVNSource where
    show (SVNSource user pass path repo) = show user

data SVNSourceInfo = SVNSourceInfo
    { svnRevision :: Int
    , svnAuthor  :: String
    , svnDate     :: ZonedTime
    }
  deriving Show
$(deriveSafeCopy 0 'base ''SVNSourceInfo)

instance Eq SVNSourceInfo where
    x == y = (svnRevision x) == (svnRevision y)

instance Ord SVNSourceInfo where
    compare x y = compare (svnRevision x) (svnRevision y)

data SVNSourceArgs = SVNSourceArgs
    { svnCommitMessage :: String
    , svnAcceptConflicts :: Bool
    }
  deriving Show

instance HaapSource SVN where
    type Source SVN = SVNSource
    type SourceInfo SVN = SVNSourceInfo
    type SourceArgs SVN = SVNSourceArgs
    getSourceWith = getSVNSourceWith
    putSourceWith = putSVNSourceWith
    getSourceInfoWith = getSVNSourceInfoWith
    
    sourcePath = svnPath

defaultSVNSourceArgs = SVNSourceArgs "system commit" True

instance Default SVNSourceArgs where
    def = defaultSVNSourceArgs
    
svnIOArgs = hiddenIOArgs
    
getSVNSourceWith :: HaapMonad m => (args -> SVNSourceArgs) -> SVNSource -> Haap p args db m ()
getSVNSourceWith getArgs s = do
    args <- Reader.reader getArgs
    let user = svnUser s
    let pass = svnPass s
    let path = svnPath s
    let repo = svnRepository s
    let (dir,name) = splitFileName path
    exists <- orLogDefault False $ runIO $ doesDirectoryExist path
    let checkout = runShWith (const svnIOArgs) $ do
        shCd dir
        shRm name
        shCommandWith svnIOArgs "svn" ["checkout",repo,"--non-interactive",name,"--username",user,"--password",pass]
    let update = runShWith (const svnIOArgs) $ do
        let conflicts = if svnAcceptConflicts args then ["--accept","theirs-full"] else []
        shCd path
        shCommandWith svnIOArgs "svn" ["cleanup"]
        res <- shCommandWith svnIOArgs "svn" (["update","--non-interactive","--username",user,"--password",pass]++conflicts)
        let okRes = resOk res
                    && not (isInfixOf "Summary of conflicts" $ Text.unpack $ resStdout res)
                    && not (isInfixOf "Summary of conflicts" $ Text.unpack $ resStderr res)
        return $ res { resExitCode = if okRes then 0 else (-1) }
    ignoreError $ if (exists)
        then do
            ok <- update
            unless (resOk ok) (checkout >> return ())
        else (checkout >> return ())
    return ()

getSVNSourceInfoWith :: HaapMonad m => (args -> SVNSourceArgs) -> SVNSource -> Haap p args db m SVNSourceInfo
getSVNSourceInfoWith getArgs s = do
    let path = svnPath s
    let user = svnUser s
    let pass = svnPass s
    info <- runShIOResultWith (const svnIOArgs) $ do
        shCd path
        shCommandWith svnIOArgs "svn" ["info","--non-interactive","--username",user,"--password",pass]
    rev <- parseInfo (resStdout info) (resStderr info)
    logRev <- runShIOResultWith (const svnIOArgs) $ do
        shCd path
        shCommandWith svnIOArgs "svn" ["log","-r",show rev,"--non-interactive","--username",user,"--password",pass]
    (author,datestr) <- parseLogRev (resStdout logRev) (resStderr logRev)
    date <- parseSVNDateCurrent datestr
    return $ SVNSourceInfo rev author date
  where
    parseInfo txt1 txt2 = case dropWhile (not . isPrefixOf "Revision:") (lines $ Text.unpack txt1) of
        (x:xs) -> case readMaybe (drop 10 x) :: Maybe Int of
            Just rev -> return rev
            Nothing -> throwError $ HaapException $ "failed to parse svn info revision for " ++ show s ++ show (Text.unpack txt1) ++ show (Text.unpack txt2)
        [] -> throwError $ HaapException $ "failed to parse svn info revision for " ++ show s ++ show (Text.unpack txt1) ++ show (Text.unpack txt2)
    parseLogRev txt1 txt2 = case tailMay (lines $ Text.unpack txt1) of
        Nothing -> return ("","")
        Just t -> case headMay t of
            Nothing -> return ("","")
            Just str -> case splitOn "|" str of
                [_,author,date,_] -> return (author,date)
                otherwise -> throwError $ HaapException $ "failed to parse svn revision log for " ++ show s ++ show (Text.unpack txt1) ++ show (Text.unpack txt2)

putSVNSourceWith :: HaapMonad m => (args -> SVNSourceArgs) -> [FilePath] -> SVNSource -> Haap p args db m ()
putSVNSourceWith getArgs files s = do
    args <- Reader.reader getArgs
    let user = svnUser s
    let pass = svnPass s
    let path = svnPath s
    let msg = svnCommitMessage args
    runShIOResultWith (const svnIOArgs) $ do
        shCd path
        forM_ files $ \file -> shCommandWith_ svnIOArgs "svn" ["add","--force","--parents",file]
        shCommandWith svnIOArgs "svn" ["commit","-m",show msg,"--non-interactive","--username",user,"--password",pass]
    return ()







