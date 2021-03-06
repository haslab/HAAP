{-
HAAP: Haskell Automated Assessment Platform

This module provides the @SVN@ plugin that provides support for _subversion_ @HaapSource@s.
-}

{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeOperators, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, EmptyDataDecls #-}

module HAAP.Sources.SVN where

import HAAP.Core
import HAAP.IO
import HAAP.Sources
import HAAP.Plugin
import HAAP.Shelly
import HAAP.Log
import HAAP.Pretty

import Data.Data
import Data.Typeable
import Data.Default
import Data.List.Split
import Data.List
import qualified Data.Text as Text
import Data.SafeCopy
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T

import Shelly (Sh)

import Control.Monad.Reader as Reader
--import Control.Monad.Except
import Control.Exception.Safe
import Control.Monad.Trans.Compose

import Text.Read hiding (lift)

import System.FilePath
import System.Directory
import System.Locale.Read 

import qualified Shelly as Sh

import GHC.Stack

import Safe

data SVN

parseSVNDateDefault :: MonadIO m => String -> m ZonedTime
parseSVNDateDefault str = parseSVNDateWith defaultTimeLocale str

parseSVNDateCurrent :: MonadIO m => String -> m ZonedTime
parseSVNDateCurrent str = do
--    liftIO $ putStrLn "getlocale"
    locale <- liftIO $ getCurrentLocale
--    liftIO $ putStrLn $ "parseSVNDateWith " ++ show locale ++ " " ++ show str
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
  deriving (Data,Typeable)
    
$(deriveSafeCopy 0 'base ''SVNSource)

instance Show SVNSource where
    show (SVNSource user pass path repo) = show user

instance Pretty SVNSource where
    pretty = viaShow

data SVNSourceInfo = SVNSourceInfo
    { svnRevision :: Int
    , svnAuthor  :: String
    , svnDate     :: ZonedTime
    }
  deriving (Show,Data,Typeable)
$(deriveSafeCopy 0 'base ''SVNSourceInfo)

instance Eq SVNSourceInfo where
    x == y = (svnRevision x) == (svnRevision y)

instance Ord SVNSourceInfo where
    compare x y = compare (svnRevision x) (svnRevision y)

instance Pretty SVNSourceInfo where
    pretty svn = pretty (svnRevision svn) <+> ";" <+> pretty (svnAuthor svn) <+> ";" <+> pretty (svnDate svn)

data SVNSourceArgs = SVNSourceArgs
    { svnCommitMessage :: String
    , svnAcceptConflicts :: Bool
    , svnCheckoutRevision :: Maybe (Either UTCTime Int)
    , svnHidden :: Bool
    , svnClean :: Bool
    }
  deriving (Show,Data,Typeable)

instance Pretty SVNSourceArgs where
    pretty = viaShow

instance HaapPlugin SVN where
    type PluginI SVN = SVNSourceArgs
    type PluginT SVN = ReaderT SVNSourceArgs
    type PluginO SVN = ()
    type PluginK SVN t m = (MonadIO m)
    
    usePlugin getArgs m = do
        args <- getArgs
        let go (ComposeT m) = do
            Reader.runReaderT m args
        x <- mapHaapMonad go m
        return (x,())

useSVN :: (HaapStack t m,PluginK SVN t m) => PluginI SVN -> Haap (PluginT SVN :..: t) m a -> Haap t m a
useSVN svnargs m = usePlugin_ (return svnargs) m

instance HaapSource SVN where
    type Source SVN = SVNSource
    type SourceInfo SVN = SVNSourceInfo
    getSource = getSVNSource
    putSource = putSVNSource
    getSourceInfo = getSVNSourceInfo
    
    sourcePath = svnPath

defaultSVNSourceArgs = SVNSourceArgs "system commit" True Nothing True False

instance Default SVNSourceArgs where
    def = defaultSVNSourceArgs
    
--instance HaapMonad m => HaapStack (ReaderT SVNSourceArgs) m where
--    liftStack = lift

instance (HaapMonad m) => HasPlugin SVN (ReaderT SVNSourceArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin SVN (ComposeT (ReaderT SVNSourceArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m
    
svnIOArgs :: SVNSourceArgs -> IOArgs
svnIOArgs args = (if svnHidden args then hiddenIOArgs else defaultIOArgs) { ioTimeout = Nothing }
    
parseSvnUTCTime :: Monad m => String -> m UTCTime
parseSvnUTCTime = parseTimeM True defaultTimeLocale "%FT%T"

showSvnUTCTime :: UTCTime -> String
showSvnUTCTime = formatTime defaultTimeLocale "%FT%T"
    
getSVNSource :: (MonadIO m,HasPlugin SVN t m) => SVNSource -> Haap t m ()
getSVNSource s = do
    args <- liftHaap $ liftPluginProxy (Proxy::Proxy SVN) $ Reader.ask
    let user = svnUser s
    let pass = svnPass s
    let path = svnPath s
    let repo = svnRepository s
    let (dir,name) = splitFileName path
    let rev = case svnCheckoutRevision args of
                    Nothing -> []
                    Just (Left utc) -> ["-r","{" ++ showSvnUTCTime utc ++ "}"]
                    Just (Right i) -> ["-r",show i]
    exists <- orLogDefault False $ runBaseIO $ doesDirectoryExist path
    let checkout = runBaseShWith' (svnIOArgs args) $ do
        shCd dir
        shRm name
        shCommandWith (svnIOArgs args) "svn" $ ["checkout",repo,"--non-interactive"] ++ rev ++ [name,"--username",user,"--password",pass]
    let update = runBaseShWith' (svnIOArgs args) $ do
        let conflicts = if svnAcceptConflicts args then ["--accept","theirs-full"] else []
        shCd path
        shCommandWith (svnIOArgs args) "svn" ["cleanup"]
        when (isJust $ svnCheckoutRevision args) $ shCommandWith_ (svnIOArgs args) "svn" ["revert","-R","."]
        res <- shCommandWith (svnIOArgs args) "svn" (["update","--non-interactive"] ++ rev ++ ["--username",user,"--password",pass]++conflicts)
        let okRes = resOk res
                    && not (isInfixOf "Summary of conflicts" $ Text.unpack $ resStdout res)
                    && not (isInfixOf "Summary of conflicts" $ Text.unpack $ resStderr res)
        return $ res { resExitCode = if okRes then 0 else (-1) }
    ignoreError $ if (exists && not (svnClean args))
        then do
            ok <- update
            unless (resOk ok) (checkout >> return ())
        else (checkout >> return ())
    return ()

getSVNSourceInfo :: (HasCallStack,MonadIO m,HasPlugin SVN t m) => SVNSource -> Haap t m SVNSourceInfo
getSVNSourceInfo s = do
    let path = svnPath s
    let user = svnUser s
    let pass = svnPass s
    --logEvent "svn plugin"
    args <- liftHaap $ liftPluginProxy (Proxy::Proxy SVN) $ Reader.ask
    let stack = maybe callStack id (ioCallStack $ svnIOArgs args)
    --logEvent "svn info"
    info <- orIOResult $ runBaseShWith' (svnIOArgs args) $ do
        shCd path
        shCommandWith (svnIOArgs args) "svn" ["info","--non-interactive","--username",user,"--password",pass]
    --logEvent "svn parseInfo"
    rev <- parseInfo stack (resStdout info) (resStderr info)
    --logEvent "svn log"
    logRev <- orIOResult $ runBaseShWith' (svnIOArgs args) $ do
        shCd path
        shCommandWith (svnIOArgs args) "svn" ["log","-r",show rev,"--non-interactive","--username",user,"--password",pass]
    --logEvent "svn parseLogRev"
    (author,datestr) <- parseLogRev stack (resStdout logRev) (resStderr logRev)
--    logEvent $ "svn parseSVNDateCurrent " ++ show datestr
    date <- liftHaap $ lift $ parseSVNDateCurrent datestr
--    logEvent "svn svnsourceinfo"
    return $ SVNSourceInfo rev author date
  where
    parseInfo stack txt1 txt2 = case dropWhile (not . isPrefixOf "Revision:") (lines $ Text.unpack txt1) of
        (x:xs) -> case readMaybe (drop 10 x) :: Maybe Int of
            Just rev -> return rev
            Nothing -> throw $ HaapException stack $ "failed to parse svn info revision for " <> T.pack (show s) <> txt1 <> txt2
        [] -> throw $ HaapException stack $ "failed to parse svn info revision for " <> T.pack (show s) <> txt1 <> txt2
    parseLogRev stack txt1 txt2 = case tailMay (lines $ Text.unpack txt1) of
        Nothing -> return ("","")
        Just t -> case headMay t of
            Nothing -> return ("","")
            Just str -> case splitOn "|" str of
                [_,author,date,_] -> return (author,date)
                otherwise -> do
                    throw $ HaapException stack $ "failed to parse svn revision log for " <> T.pack (show s) <> txt1 <> txt2

putSVNSource :: (MonadIO m,HasPlugin SVN t m) => [FilePath] -> SVNSource -> Haap t m ()
putSVNSource files s = do
    args <- liftHaap $ liftPluginProxy (Proxy::Proxy SVN) $ Reader.ask
    let user = svnUser s
    let pass = svnPass s
    let path = svnPath s
    let msg = svnCommitMessage args
    orIOResult $ runBaseShWith' (svnIOArgs args) $ do
        shCd path
        forM_ files $ \file -> shCommandWith_ (svnIOArgs args) "svn" ["add","--force","--parents",file]
        shCommandWith (svnIOArgs args) "svn" ["commit","-m",show msg,"--non-interactive","--username",user,"--password",pass]
    return ()







