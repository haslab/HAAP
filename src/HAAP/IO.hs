{-
HAAP: Haskell Automated Assessment Platform

This module provides basic IO functionalities.
-}

{-# LANGUAGE BangPatterns, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}

module HAAP.IO where

import HAAP.Core
import HAAP.Log
import HAAP.Pretty
import HAAP.Plugin

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.DeepSeq as DeepSeq
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as Reader
--import Control.Monad.Catch
--import Control.Monad.Except
import Control.Monad
import qualified Control.Exception as E
import Control.Exception.Safe

import Data.Default
import Data.Semigroup
import Data.Text (Text(..))
import qualified Data.Text as Text
import Data.Foldable
import Data.Typeable
import Data.Proxy
import Data.String
import Data.Bifunctor (bimap)
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

--import System.Timeout
import System.FilePath
import System.Exit
import System.Process
import System.Directory
import System.Environment
import System.FilePath.GlobPattern

import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

import Test.QuickCheck

import Text.Read hiding (lift)

import GHC.Stack
import GHC.Generics
import System.IO

import Shelly (Sh(..),catchany_sh)
import qualified Shelly as Sh

instance (MonadIO m,HaapStack t m) => MonadIO (Haap t m) where
    {-# INLINE liftIO #-}
    liftIO io = haapLiftIO $ runIOCore (def) io

data Sandbox
    = NoSandbox
    | Sandbox (Maybe FilePath) -- optional config file

mapSandboxCfg :: (FilePath -> FilePath) -> Sandbox -> Sandbox
mapSandboxCfg f NoSandbox = NoSandbox
mapSandboxCfg f (Sandbox mb) = Sandbox $ fmap f mb

data IOArgs = IOArgs
    { ioTimeout :: Maybe Int -- in seconds
    , ioSilent :: Bool -- run silently without printing to stderr or stdout
    , ioStdin :: Maybe Text -- input file or handle for processes
    , ioSandbox :: Sandbox -- run inside a cabal sandbox (with given config file) or not; relative to the current path
    , ioEscaping :: Bool -- escape shell characters or not
    , ioEnv :: [(String,FilePath)] -- additional environment variables
    , ioCmd :: Maybe String -- environment command (such as env, bash)
    , ioHidden :: Bool -- run without reporting errors 
    }

addIOCmd :: Maybe String -> IOArgs -> IOArgs
addIOCmd Nothing io = io
addIOCmd (Just cmd) io = io { ioCmd = Just cmd }

addIOEnv :: [(String,FilePath)] -> IOArgs -> IOArgs
addIOEnv xs io = io { ioEnv = ioEnv io ++ xs }

data IOResult = IOResult
    { resExitCode :: Int
    , resStdout :: Text
    , resStderr :: Text
    } deriving (Generic)
instance NFData IOResult where

resOk :: IOResult -> Bool
resOk = (==0) . resExitCode

exitCode :: ExitCode -> Int
exitCode (ExitFailure i) = i
exitCode (ExitSuccess) = 0

instance Semigroup IOResult where
    (<>) = mappend

instance Monoid IOResult where
    mempty = IOResult 0 Text.empty Text.empty
    mappend x y = IOResult
        (resExitCode x `min` resExitCode y)
        (resStdout x `Text.append` resStdout y)
        (resStdout x `Text.append` resStdout y)

instance Pretty IOResult where
    pretty io = string "Output:" $+$ text (resStdout io)
           $+$ string "Errors:" $+$ text (resStderr io)
           $+$ string "Exit Code:" <+> pretty (resExitCode io)

defaultIOArgs :: IOArgs
defaultIOArgs = IOArgs (Just 60) False Nothing NoSandbox True [] Nothing False

hiddenIOArgs :: IOArgs
hiddenIOArgs = defaultIOArgs {ioHidden = True, ioSilent = True }

instance Default IOArgs where
    def = defaultIOArgs

runIOWithTimeout :: (HaapPureStack t m IO,MonadIO m) => Int -> Haap t IO a -> Haap t m a
runIOWithTimeout timeout m = runIOWith (args) m
    where
    args = def { ioTimeout = Just timeout }
    
runBaseIOWithTimeout :: (HaapStack t m,MonadIO m) => Int -> IO a -> Haap t m a
runBaseIOWithTimeout timeout m = runBaseIOWith (args) m
    where
    args = def { ioTimeout = Just timeout }

runBaseIOWith :: (HaapStack t m,MonadIO m) => IOArgs -> IO a -> Haap t m a
runBaseIOWith args io = do
    haapLiftIO $ runIOCore args io

runIOWith :: (HaapPureStack t m IO,MonadIO m) => IOArgs -> Haap t IO a -> Haap t m a
runIOWith args io = do
    st <- liftWith' $ \run -> do
        st <- runIOCore args $ run io
        return st
    restoreT' $ return st

runBaseIOWith' :: (NFData a,HaapStack t m,MonadIO m) => IOArgs -> IO a -> Haap t m a
runBaseIOWith' args io = do
    haapLiftIO $ forceM $ runIOCore args io

runIOWith' :: (HaapPureStack t m IO,MonadIO m,NFData a) => IOArgs -> Haap t IO a -> Haap t m a
runIOWith' args io = forceM $ runIOWith args io

runBaseIO :: (HaapStack t m,MonadIO m) => IO a -> Haap t m a
runBaseIO = runBaseIOWith (defaultIOArgs)

runIO :: (HaapPureStack t m IO,MonadIO m) => Haap t IO a -> Haap t m a
runIO = runIOWith (defaultIOArgs)

runIOExit :: (HaapPureStack t m IO,MonadIO m) => Haap t IO () -> Haap t m ExitCode
runIOExit m = runIOWith (defaultIOArgs) (catch (m >> lift exitSuccess) (lift . catchExit))
    where
    catchExit :: ExitCode -> IO ExitCode
    catchExit e = return e

ioExit :: IO () -> IO ExitCode
ioExit m = catch (m >> exitSuccess) catchExit
    where
    catchExit :: ExitCode -> IO ExitCode
    catchExit e = return e

haapRetry :: (MonadIO m,HaapStack t m) => Int -> Haap t m a -> Haap t m a
haapRetry 0 m = m
haapRetry i m = orDo (\e -> logError e >> haapRetry (pred i) m) m

ioCommand_ :: String -> [String] -> IO ()
ioCommand_ name args = ioCommandWith def name args >> return ()

ioCommandWith_ :: IOArgs -> String -> [String] -> IO ()
ioCommandWith_ ioargs name args = ioCommandWith ioargs name args >> return ()

ioCommandWith :: IOArgs -> String -> [String] -> IO IOResult
ioCommandWith ioargs name args = addHiddenIO $ do
    forM_ (ioEnv ioargs) $ \(evar,epath) -> setEnv evar epath
    let stdin = maybe [] Text.unpack $ ioStdin ioargs
    let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
    (exit,stdout,stderr) <- readProcessWithExitCode (head cmds) (tail cmds) stdin
    unless (ioSilent ioargs || ioHidden ioargs) $ do
        putStrLn $ "Running IO: " ++ unwords cmds
        putStrLn $ stderr
        putStrLn $ stdout
    return $ IOResult (exitCode exit) (Text.pack stdout) (Text.pack stderr)
  where
    addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
    addTimeout Nothing cmds = cmds
    addTimeout (Just secs) cmds = ["timeout",prettyString secs++"s"]++cmds
--    addRedir cmds = if ioHidden ioargs then cmds ++ ["2>/dev/null"] else cmds
    addSandbox NoSandbox cmds = cmds
    addSandbox (Sandbox Nothing) cmds = ["cabal","exec","--"]++cmds
    addSandbox (Sandbox (Just cfg)) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds
    addHiddenIO m = if ioHidden ioargs then Sh.catchany m (\err -> evaluateM $! mempty) else m

haapLiftIO :: (HaapStack t m,MonadIO m) => IO a -> Haap t m a
haapLiftIO io = catchAny (evaluateM $! lift $ liftIO io) (\e -> throw $ HaapIOException e)

runIOCore :: MonadIO m => IOArgs -> IO a -> m a
runIOCore args io = case ioTimeout args of
    Nothing -> liftIO $ io
    Just secs -> do
        mb <- liftIO $ timeoutIO (secs * 10^6) io
        case mb of
            Nothing -> error $ prettyString $ HaapTimeout callStack secs
            Just a -> return a

runBaseIO' :: (HaapStack t m,MonadIO m,NFData a) => IO a -> Haap t m a
runBaseIO' = runBaseIOWith' (defaultIOArgs)

runIO' :: (HaapPureStack t m IO,MonadIO m,NFData a) => Haap t IO a -> Haap t m a
runIO' = runIOWith' (defaultIOArgs)

orEither :: (MonadIO m,HaapStack t m) => Haap t m a -> Haap t m (Either SomeException a)
orEither m = orDo (\e -> return $ Left e) (liftM Right m)

orLogEither :: (MonadIO m,HaapStack t m) => Haap t m a -> Haap t m (Either SomeException a)
orLogEither m = orDo (\e -> logEvent (prettyText e) >> return (Left e)) (liftM Right m)

orMaybe :: (MonadIO m,HaapStack t m) => Haap t m a -> Haap t m (Maybe a)
orMaybe m = orDo (\e -> return Nothing) (liftM Just m)

orLogMaybe :: (MonadIO m,HaapStack t m) => Haap t m a -> Haap t m (Maybe a)
orLogMaybe m = orDo (\e -> logEvent (prettyText e) >> return Nothing) (liftM Just m)

orDo :: (MonadIO m,HaapStack t m) => (SomeException -> Haap t m a) -> Haap t m a -> Haap t m a
orDo ex m = catchAny (evaluateM m) ex

orLogDo :: (MonadIO m,HaapStack t m) => (SomeException -> Haap t m a) -> Haap t m a -> Haap t m a
orLogDo f m = orDo (\e -> logEvent (prettyText e) >> f e) m

orDoIO :: (SomeException -> IO a) -> IO a -> IO a
orDoIO ex m = catchAny (evaluateM m) ex

orLogDefault :: (MonadIO m,HaapStack t m) => a -> Haap t m a -> Haap t m a
orLogDefault a m = orDo (\e -> logEvent (prettyText e) >> return a) m

orDefault :: (MonadIO m,HaapStack t m) => a -> Haap t m a -> Haap t m a
orDefault a m = orDo (\e -> return a) m

orMaybeIO :: IO a -> IO (Maybe a)
orMaybeIO m = catchAny (evaluateM $! liftM Just m) (\err -> return Nothing)

orError :: (MonadIO m,HaapStack t m) => Haap t m a -> Haap t m (Either a SomeException)
orError m = orDo (return . Right) (liftM Left m)

orDo' :: (MonadIO m,HaapStack t m,NFData a) => (SomeException -> Haap t m a) -> Haap t m a -> Haap t m a
orDo' ex m = catchAny (forceM m) ex

ignoreError :: (MonadIO m,HaapStack t m) => Haap t m () -> Haap t m ()
ignoreError m = orDo' (\e -> logEvent (prettyText e)) m

addMessageToError :: (MonadIO m,HaapStack t m) => T.Text -> Haap t m a -> Haap t m a
addMessageToError msg m = orDo (\e -> throw $ HaapException $ msg <> prettyText e) m

orLogError :: (MonadIO m,IsString str,HaapStack t m) => Haap t m str -> Haap t m str
orLogError m = orDo (\e -> logEvent (prettyText e) >> return (fromString $ prettyString e)) m

forceHaap :: (NFData a,HaapStack t m,MonadIO m) => a -> Haap t m a
forceHaap x = liftIO $! E.evaluate $! force x

forceM :: (MonadIO m,NFData a) => m a -> m a
forceM m = do
    !x <- m
    liftIO $! E.evaluate $! DeepSeq.force x
    
evaluate :: MonadIO m => a -> m a
evaluate x = do
    liftIO $! E.evaluate $! x
    
evaluateM :: MonadIO m => m a -> m a
evaluateM m = do
    !x <- m
    liftIO $! E.evaluate $! x

equalPathIO :: FilePath -> FilePath -> IO Bool
equalPathIO x y = do
    x' <- canonicalizePath x
    y' <- canonicalizePath y
    return $ x' `equalFilePath` y'

forAllIO :: Int -> Gen a -> (a -> IO b) -> IO [b]
forAllIO num gen f = do
    xs <- generate $ vectorOf num gen
    mapM f xs

orIOResult :: (MonadIO m,HaapStack t m) => Haap t m IOResult -> Haap t m IOResult
orIOResult m = orDo (\err -> return $ IOResult (-1) Text.empty (prettyText err)) m

-- timeout from System.Timeout sometimes fails to halt the computation, so we wrap it as an async computation
--timeoutIO :: Int -> IO a -> IO (Maybe a)
--timeoutIO i f = liftM join $ asyncTimeout i $ timeout i f

timeoutIO :: Int -> IO a -> IO (Maybe a)
timeoutIO i f =
  withAsync f $ \a1 ->
  withAsync (threadDelay i) $ \a2 ->
  liftM (either Just (const Nothing)) $ race (wait a1) (wait a2)




