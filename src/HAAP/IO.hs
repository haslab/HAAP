{-# LANGUAGE ScopedTypeVariables #-}

module HAAP.IO where

import HAAP.Core
import HAAP.Log
import HAAP.Pretty

import Control.DeepSeq as DeepSeq
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as Reader
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad

import Data.Default
import Data.Text (Text(..))
import qualified Data.Text as Text
import Data.Foldable

import System.Timeout
import System.FilePath
import System.Exit
import System.Directory

import GHC.Stack

import Shelly (Sh(..))
import qualified Shelly as Sh

instance MonadIO (Haap p args db) where
    {-# INLINE liftIO #-}
    liftIO io = runIOWith def io

data IOArgs = IOArgs
    { ioTimeout :: Maybe Int -- in seconds
    , ioSilent :: Bool -- run silently without printing to stderr or stdout
    , ioStdin :: Maybe Text -- input file or handle for processes
    , ioSandbox :: Bool -- run inside a cabal snadbox or not
    , ioEscaping :: Bool -- escape shell characters or not
    }

data IOResult = IOResult
    { resExitCode :: Int
    , resStdout :: Text
    , resStderr :: Text
    }

defaultIOArgs :: IOArgs
defaultIOArgs = IOArgs Nothing False Nothing False False

instance Default IOArgs where
    def = defaultIOArgs

runIOWith :: (args -> IOArgs) -> IO a -> Haap p args db a
runIOWith getArgs io = do
    args <- Reader.reader getArgs
    runIOCore args io

runIOWith' :: NFData a => (args -> IOArgs) -> IO a -> Haap p args db a
runIOWith' getArgs io = forceM $ runIOWith getArgs io

runIO :: IO a -> Haap p args db a
runIO = runIOWith (const defaultIOArgs)

runIOExit :: IO () -> Haap p args db ExitCode
runIOExit m = runIOWith (const defaultIOArgs) (catch (m >> exitSuccess) catchExit)
    where
    catchExit :: ExitCode -> IO ExitCode
    catchExit e = return e

runShWith :: (args -> IOArgs) -> Sh a -> Haap p args db a
runShWith getArgs io = do
    args <- Reader.reader getArgs
    runShCore args io

runSh :: Sh a -> Haap p args db a
runSh = runShWith (const defaultIOArgs)

shCommandWith :: IOArgs -> String -> [String] -> Sh IOResult
shCommandWith ioargs name args  = do
    forM_ (ioStdin ioargs) Sh.setStdin
    let cmds = addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
    stdout <- Sh.errExit False $ Sh.run (shFromFilePath $ head cmds) (map Text.pack $ tail cmds)
    stderr <- Sh.lastStderr
    exit <- Sh.lastExitCode
    return $ IOResult exit stdout stderr
  where
    addTimeout Nothing cmds = cmds
    addTimeout (Just secs) cmds = ["timeout",pretty secs++"s"]++cmds
    addSandbox False cmds = cmds
    addSandbox True cmds = ["cabal","exec","-- " ++ unwords cmds]

haapLiftIO :: IO a -> Haap p args db a
haapLiftIO io = Haap $ catch (liftIO io) (\(e::SomeException) -> throwError $ HaapIOException e)

runShCore :: IOArgs -> Sh a -> Haap p args db a
runShCore args sh = case ioTimeout args of
    Nothing -> haapLiftIO io
    Just secs -> do
        mb <- haapLiftIO $ timeout (secs * 10^6) io
        case mb of
            Nothing -> throwError $ HaapTimeout callStack secs
            Just a -> return a
  where
    io = Sh.shelly $ silent $ Sh.escaping (ioEscaping args) sh
    silent = if ioSilent args then Sh.silently else Sh.verbosely

runIOCore :: IOArgs -> IO a -> Haap p args db a
runIOCore args io = case ioTimeout args of
    Nothing -> haapLiftIO io
    Just secs -> do
        mb <- haapLiftIO $ timeout (secs * 10^6) io
        case mb of
            Nothing -> throwError $ HaapTimeout callStack secs
            Just a -> return a

runIO' :: NFData a => IO a -> Haap p args db a
runIO' = runIOWith' (const defaultIOArgs)

orDefault :: (HaapException -> Haap p args db a) -> Haap p args db a -> Haap p args db a
orDefault ex m = catchError m ex

orError :: Haap p args db a -> Haap p args db (Either a HaapException)
orError m = orDefault (return . Right) (liftM Left m)

orDefault' :: NFData a => (HaapException -> Haap p args db a) -> Haap p args db a -> Haap p args db a
orDefault' ex m = catchError (forceM m) ex

ignoreError :: Haap p args db () -> Haap p args db ()
ignoreError m = orDefault (\e -> logEvent (show e)) m

forceM :: (Monad m,NFData a) => m a -> m a
forceM m = do
    x <- m
    return $! DeepSeq.force x

shFromFilePath :: FilePath -> Sh.FilePath
shFromFilePath = Sh.fromText . Text.pack

shCd :: FilePath -> Sh ()
shCd = Sh.cd . shFromFilePath

copyRecursive :: FilePath -> FilePath -> Haap pa rgs db ()
copyRecursive from to = do
    isfromdir <- runIO $ doesDirectoryExist from
    istodir <- runIO $ doesDirectoryExist to
    if (isfromdir && istodir)
        then do
            froms <- runIO $ listDirectory from
            forM_ froms $ \x -> copyRecursive (from </> x) to
        else runSh $ Sh.cp_r (shFromFilePath from) (shFromFilePath to)

data GHCArgs = GHCArgs
    { ghcSafe :: Bool -- compile with the -XSafe extension
    , ghcHpc :: Bool -- compile for hpc
    , ghcArgs :: [String] -- additional flags
    }

instance Default GHCArgs where
    def = GHCArgs True False []

shCompileWith :: IOArgs -> GHCArgs -> [FilePath] -> Sh IOResult
shCompileWith io ghc ins = shCommandWith io "ghc" (addHpc (ghcHpc ghc) $ addSafe (ghcSafe ghc) ins)
    where
    addSafe True cmds = "-XSafe" : cmds
    addSafe False cmds = cmds
    addHpc True  cmds = "-fhpc" : cmds
    addHpc False cmds = cmds
        
        
        
        

