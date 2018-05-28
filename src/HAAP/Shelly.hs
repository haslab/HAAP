{-
HAAP: Haskell Automated Assessment Platform

This module provides shell scripting functionalities.
-}

{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, TypeOperators, UndecidableInstances, ScopedTypeVariables, EmptyDataDecls, GADTs, TypeFamilies, FlexibleContexts #-}

module HAAP.Shelly where

import HAAP.Core
import HAAP.Log
import HAAP.Pretty
import HAAP.Plugin
import HAAP.IO

import Shelly (Sh(..),catchany_sh)
import qualified Shelly as Sh

import Control.Monad.Identity
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State as State
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Monad.Morph
import qualified Control.Monad.Except as Except
import qualified Control.Monad.RWS as RWS
import Control.Monad.Except
import Control.Monad
import Control.Exception
import Control.Monad.Catch (MonadCatch(..),MonadThrow(..))

import System.Timeout
import System.FilePath
import System.Exit
import System.Process
import System.Directory
import System.Environment
import System.FilePath.GlobPattern

import Data.Default
import Data.Text (Text(..))
import qualified Data.Text as Text
import Data.Foldable
import Data.Typeable
import Data.Proxy
import Data.String
import Data.Bifunctor (bimap)
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Text.Read

import System.IO

import GHC.Stack

runBaseSh :: (MonadIO m,HaapStack t m) => Sh a -> Haap t m a
runBaseSh = runBaseShWith def

runSh :: (MonadIO m,HaapPureLiftT (Haap t) m Sh,HaapPureRestoreT (Haap t) m) => Haap t Sh a -> Haap t m a
runSh = runShWith def

runBaseShWith :: (MonadIO m,HaapStack t m) => IOArgs -> Sh a -> Haap t m a
runBaseShWith ioargs msh = do
    haapLiftIO $ runShCoreIO ioargs msh

runShWith :: (MonadIO m,HaapPureLiftT (Haap t) m Sh,HaapPureRestoreT (Haap t) m) => IOArgs -> Haap t Sh a -> Haap t m a
runShWith ioargs msh = do
    st <- liftWithPluginT $ \run -> do
        st <- liftIO $ runShCoreIO ioargs $ run msh
        return st
    restorePluginT $ return st

runShWithTimeout :: (MonadIO m,HaapPureLiftT (Haap t) m Sh,HaapPureRestoreT (Haap t) m) => Int -> Haap t Sh a -> Haap t m a
runShWithTimeout timeout m = runShWith args m
    where
    args = def { ioTimeout = Just timeout }

runShCoreIO :: IOArgs -> Sh a -> IO a
runShCoreIO args sh = case ioTimeout args of
    Nothing -> io
    Just secs -> do
        mb <- timeoutIO (secs * 10^6) io
        case mb of
            Nothing -> error $ pretty $ HaapTimeout callStack secs
            Just a -> return a
  where
    io = Sh.shelly $ silent $ Sh.escaping (ioEscaping args) sh
    silent = if ioHidden args
        then hiddenSh
        else if ioSilent args then Sh.silently else Sh.verbosely

hiddenSh :: Sh a -> Sh a
hiddenSh = \m -> Sh.catchany_sh (hide m) (const $ return $ error "result is hidden!")
    where
    hide = Sh.print_commands False . Sh.log_stderr_with (const $ return ()) . Sh.silently . Sh.tracing False

-- * Shelly utilities

liftSh :: HaapStack t Sh => Sh a -> Haap t Sh a
liftSh m = liftStack m

liftShOp :: (HaapPurePluginT t Sh) => (Sh (StPluginT (Haap t) a) -> Sh (StPluginT (Haap t) c)) -> Haap t Sh a -> Haap t Sh c
liftShOp f m = liftWithPluginT (\run -> f $ run m) >>= restorePluginT . return

orErrorWritePage :: (HaapStack t m,Out a,MonadIO m) => FilePath -> a -> Haap t m a -> Haap t m a
orErrorWritePage path def m = orDo go $ do
    x <- m
    ok <- orLogDefault False $ runBaseIO $ doesFileExist path
    unless ok $ orLogDefault () $ runBaseSh $ shWriteFile path $ pretty x
    return x
  where
    go e = do
        runBaseIO $ writeFile path $ pretty e
        return def

shExec :: String -> Sh FilePath
shExec exec = do
    mb <- Sh.which (shFromFilePath exec)
    case mb of
        Nothing -> shAbsolutePath exec
        Just exec' -> return $ shToFilePath exec'

shAbsolutePath :: FilePath -> Sh FilePath
shAbsolutePath = liftM shToFilePath . Sh.absPath . shFromFilePath

shCommand_ :: String -> [String] -> Sh ()
shCommand_ = shCommandWith_ defaultIOArgs

shCommandWith_ :: IOArgs -> String -> [String] -> Sh ()
shCommandWith_ ioargs name args  = do
    forM_ (ioStdin ioargs) Sh.setStdin
    forM_ (ioEnv ioargs) $ \(evar,epath) -> Sh.setenv (Text.pack evar) (Text.pack epath)
    let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
    Sh.run_ (shFromFilePath $ head cmds) (map Text.pack $ tail cmds)
  where
    addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
    addTimeout Nothing cmds = cmds
    addTimeout (Just secs) cmds = ["timeout",pretty secs++"s"]++cmds
--    addRedir cmds = if ioHidden ioargs then cmds ++ ["2>/dev/null"] else cmds
    addSandbox NoSandbox cmds = cmds
    addSandbox (Sandbox Nothing) cmds = ["cabal","exec","--"]++cmds
    addSandbox (Sandbox (Just cfg)) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds
    
shCommandToFileWith_ :: IOArgs -> String -> [String] -> FilePath -> Sh ()
shCommandToFileWith_ ioargs name args file = do
    forM_ (ioStdin ioargs) Sh.setStdin
    forM_ (ioEnv ioargs) $ \(evar,epath) -> Sh.setenv (Text.pack evar) (Text.pack epath)
    let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
    Sh.runHandle (shFromFilePath $ head cmds) (map Text.pack $ tail cmds) handle
  where
    addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
    addTimeout Nothing cmds = cmds
    addTimeout (Just secs) cmds = ["timeout",pretty secs++"s"]++cmds
--    addRedir cmds = if ioHidden ioargs then cmds ++ ["2>/dev/null"] else cmds
    addSandbox NoSandbox cmds = cmds
    addSandbox (Sandbox Nothing) cmds = ["cabal","exec","--"]++cmds
    addSandbox (Sandbox (Just cfg)) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds
    handle h = do
        bs <- liftIO $ BS.hGetContents h
        liftIO $ BS.writeFile file bs

shCommand :: String -> [String] -> Sh IOResult
shCommand = shCommandWith defaultIOArgs

shCommandWith :: IOArgs -> String -> [String] -> Sh IOResult
shCommandWith ioargs name args = shCommandWith' ioargs name args
    where
    shCommandWith' :: IOArgs -> String -> [String] -> Sh IOResult
    shCommandWith' ioargs name args = do
        forM_ (ioStdin ioargs) Sh.setStdin
        forM_ (ioEnv ioargs) $ \(evar,epath) -> Sh.setenv (Text.pack evar) (Text.pack epath)
        let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
        stdout <- Sh.errExit False $ Sh.run (shFromFilePath $ head cmds) (map Text.pack $ tail cmds)
        stderr <- if ioHidden ioargs then return (Text.pack "hidden") else Sh.lastStderr
        exit <- Sh.lastExitCode
        return $ IOResult exit stdout stderr
      where
        addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
        addTimeout Nothing cmds = cmds
        addTimeout (Just secs) cmds = ["timeout",pretty secs++"s"]++cmds
        addSandbox NoSandbox cmds = cmds
        addSandbox (Sandbox Nothing) cmds = ["cabal","exec","--"]++cmds
        addSandbox (Sandbox (Just cfg)) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds

shPipeBinaryWith :: Binary a => IOArgs -> String -> [String] -> Sh a
shPipeBinaryWith ioargs name args = shCommandHandleWith ioargs name args handle
    where
    handle h = do
        bs <- liftIO $ BS.hGetContents h
        return $ decode bs

shCommandHandleWith :: IOArgs -> String -> [String] -> (Handle -> Sh a) -> Sh a
shCommandHandleWith ioargs name args handle = do
    forM_ (ioStdin ioargs) Sh.setStdin
    forM_ (ioEnv ioargs) $ \(evar,epath) -> Sh.setenv (Text.pack evar) (Text.pack epath)
    let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
    a <- Sh.errExit False $ Sh.runHandle (shFromFilePath $ head cmds) (map Text.pack $ tail cmds) handle
    return a
  where
    addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
    addTimeout Nothing cmds = cmds
    addTimeout (Just secs) cmds = ["timeout",pretty secs++"s"]++cmds
    addSandbox NoSandbox cmds = cmds
    addSandbox (Sandbox Nothing) cmds = ["cabal","exec","--"]++cmds
    addSandbox (Sandbox (Just cfg)) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds

shPipeWith :: (Show a,Read b,Typeable b) => IOArgs -> String -> [String] -> a -> Sh b
shPipeWith io n args x = shPipeWithType io n args x Proxy
    where
    shPipeWithType :: (Show a,Read b,Typeable b) => IOArgs -> String -> [String] -> a -> Proxy b -> Sh b
    shPipeWithType io n args x (_::Proxy b) = do
        let typeb = typeOf (error "shPipeWith"::b)
        let io' = io { ioStdin = Just $ Text.pack $ show x }
        res <- orEitherSh $ shCommandWith io' n args
        case res of
            Left err -> error $ "error...\n" ++ pretty err ++ "\n...on parsing result as type...\n" ++ show typeb
            Right res -> do
                let out = (Text.unpack . resStdout) res
                case readMaybe out of
                    Nothing -> error $ "failed to parse result...\n" ++ show out ++ "\n...as type...\n" ++ show typeb ++ "\n" ++ pretty res
                    Just y -> return y

orEitherSh :: Sh a -> Sh (Either SomeException a)
orEitherSh m = catchany_sh (liftM Right m) (\err -> return $ Left err)

shWriteFile :: FilePath -> String -> Sh ()
shWriteFile path ct = do
    Sh.mkdir_p $ shFromFilePath $ takeDirectory path
    Sh.writefile (shFromFilePath path) (Text.pack ct)

shFromFilePath :: FilePath -> Sh.FilePath
shFromFilePath = Sh.fromText . Text.pack

shToFilePath :: Sh.FilePath -> FilePath
shToFilePath = Text.unpack . Sh.toTextIgnore

shCd :: FilePath -> Sh ()
shCd = Sh.cd . shFromFilePath

shMkDir :: FilePath -> Sh ()
shMkDir = Sh.mkdir_p . shFromFilePath

shCp :: FilePath -> FilePath -> Sh ()
shCp from to = Sh.cp_r (shFromFilePath from) (shFromFilePath to)

shRm :: FilePath -> Sh ()
shRm from = Sh.rm_rf (shFromFilePath from)

shLs :: FilePath -> Sh [FilePath]
shLs = liftM (map shToFilePath) . Sh.ls . shFromFilePath

shCanonalize :: FilePath -> Sh FilePath
shCanonalize = liftM shToFilePath . Sh.canonicalize . shFromFilePath

shDoesDirectoryExist :: FilePath -> Sh Bool
shDoesDirectoryExist = Sh.test_d . shFromFilePath

shDoesFileExist :: FilePath -> Sh Bool
shDoesFileExist = Sh.test_f . shFromFilePath

shCpRecursive :: FilePath -> FilePath -> Sh ()
shCpRecursive = shRecursive shCp

shRecursive :: (FilePath -> FilePath -> Sh ()) -> FilePath -> FilePath -> Sh ()
shRecursive op from to = do
--    Sh.liftIO $ putStrLn $ "shRecursive " ++ show from
    isfromdir <- shDoesDirectoryExist from
    shMkDir $ takeDirectory to
    if isfromdir
        then do
            istodir <- shDoesDirectoryExist to
            unless (istodir) $ shMkDir to
            froms <- shLs from
            forM_ froms $ \x -> shRecursive op (from </> x) to       
        else op from to

infix ~~~
(~~~) :: Sh.FilePath -> GlobPattern -> Sh Bool
f ~~~ p = return $ (shToFilePath f) ~~ p
        
shFindGlob :: FilePath -> Sh [FilePath]
shFindGlob path = do
    xs <- Sh.findWhen (\x -> x ~~~ (dir </> exec)) (shFromFilePath dir)
    return $ map shToFilePath xs
  where
    (dir,exec) = splitFileName path      

shFindWhen :: (FilePath -> Sh Bool) -> FilePath -> Sh [FilePath]
shFindWhen p path = do
    xs <- Sh.findWhen (p . shToFilePath) (shFromFilePath dir)
    return $ map shToFilePath xs
  where
    (dir,exec) = splitFileName path      

equalPathSh :: FilePath -> FilePath -> Sh Bool
equalPathSh x y = do
    x' <- shCanonalize x
    y' <- shCanonalize y
    return $ equalFilePath x y
