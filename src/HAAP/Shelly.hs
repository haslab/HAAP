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

import Shelly (Sh(..))
import qualified Shelly as Sh

import Control.Monad.Identity
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State as State
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Control.Monad.Morph
--import qualified Control.Monad.Except as Except
import qualified Control.Monad.RWS as RWS
--import Control.Monad.Except
import Control.Monad
--import Control.Exception
--import Control.Monad.Catch (MonadCatch(..),MonadThrow(..))
import Control.DeepSeq
import Control.Exception.Safe

--import System.Timeout
import System.FilePath
import System.Exit
import System.Process
import System.Directory
import System.Environment
import System.FilePath.GlobPattern
import qualified System.IO.Strict as Strict

import Data.Default
import Data.Text (Text(..))
import qualified Data.Text as T
import Data.Foldable
import Data.Typeable
import Data.Proxy
import Data.String
import Data.Bifunctor (bimap)
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Text.Read hiding (lift)
import qualified Data.Text.IO as T

import System.IO

import GHC.Stack

runBaseSh :: (MonadIO m,HaapStack t m) => Sh a -> Haap t m a
runBaseSh = runBaseShWith def

runBaseSh' :: (MonadIO m,HaapStack t m,NFData a) => Sh a -> Haap t m a
runBaseSh' = runBaseShWith' def

runSh :: (MonadIO m,HaapPureStack t m Sh) => Haap t Sh a -> Haap t m a
runSh = runShWith def

runBaseShWith :: (MonadIO m,HaapStack t m) => IOArgs -> Sh a -> Haap t m a
runBaseShWith ioargs msh = do
    haapLiftIO $ runShCoreIO ioargs msh

runBaseShWith' :: (MonadIO m,HaapStack t m,NFData a) => IOArgs -> Sh a -> Haap t m a
runBaseShWith' ioargs msh = do
    haapLiftIO $! forceM $! runShCoreIO ioargs msh

runShWith :: (MonadIO m,HaapPureStack t m Sh) => IOArgs -> Haap t Sh a -> Haap t m a
runShWith ioargs msh = do
    st <- liftWith' $ \run -> do
        st <- liftIO $ runShCoreIO ioargs $ run msh
        return st
    restoreT' $ return st

runShWithTimeout :: (MonadIO m,HaapPureStack t m Sh) => Int -> Haap t Sh a -> Haap t m a
runShWithTimeout timeout m = runShWith args m
    where
    args = def { ioTimeout = Just timeout }

runShCoreIO' :: NFData a => IOArgs -> Sh a -> IO a
runShCoreIO' args sh = forceM $ runShCoreIO args sh

runShCoreIO :: IOArgs -> Sh a -> IO a
runShCoreIO args sh = case ioTimeout args of
    Nothing -> io
    Just secs -> do
        mb <- timeoutIO (secs * 10^6) io
        case mb of
            Nothing -> error $ prettyString $ HaapTimeout callStack secs
            Just a -> return a
  where
    io = Sh.shelly $ silent $ Sh.escaping (ioEscaping args) sh
    silent = if ioHidden args
        then hiddenSh
        else if ioSilent args then Sh.silently else Sh.verbosely

hiddenSh :: Sh a -> Sh a
hiddenSh = \m -> catchAnySh (hide m) (const $ return $ error "result is hidden!")
    where
    hide = Sh.print_commands False . Sh.log_stderr_with (const $ return ()) . Sh.silently . Sh.tracing False

-- * Shelly utilities

liftSh :: HaapStack t Sh => Sh a -> Haap t Sh a
liftSh m = lift m

liftShOp :: (HaapPureStack t Sh Sh) => (Sh (StT' (Haap t) a) -> Sh (StT' (Haap t) c)) -> Haap t Sh a -> Haap t Sh c
liftShOp f m = liftWith' (\run -> f $ run m) >>= restoreT' . return

orErrorWritePage :: (HaapStack t m,Pretty a,MonadIO m) => FilePath -> a -> Haap t m a -> Haap t m a
orErrorWritePage path def m = orDo go $ do
    x <- m
    ok <- orLogDefault False $ runBaseIO $ doesFileExist path
    unless ok $ orLogDefault () $ runBaseSh $ shWriteFile' path $ prettyText x
    return x
  where
    go e = do
        runBaseIO $ T.writeFile path $ prettyText e
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
    forM_ (ioEnv ioargs) $ \(evar,epath) -> Sh.setenv (T.pack evar) (T.pack epath)
    let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
    Sh.run_ (shFromFilePath $ head cmds) (map T.pack $ tail cmds)
  where
    addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
    addTimeout Nothing cmds = cmds
    addTimeout (Just secs) cmds = ["timeout",prettyString secs++"s"]++cmds
--    addRedir cmds = if ioHidden ioargs then cmds ++ ["2>/dev/null"] else cmds
    addSandbox NoSandbox cmds = cmds
    addSandbox (Sandbox Nothing) cmds = ["cabal","exec","--"]++cmds
    addSandbox (Sandbox (Just cfg)) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds
    
shCommandToFileWith_ :: IOArgs -> String -> [String] -> FilePath -> Sh ()
shCommandToFileWith_ ioargs name args file = do
    forM_ (ioStdin ioargs) Sh.setStdin
    forM_ (ioEnv ioargs) $ \(evar,epath) -> Sh.setenv (T.pack evar) (T.pack epath)
    let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
    Sh.runHandle (shFromFilePath $ head cmds) (map T.pack $ tail cmds) handle
  where
    addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
    addTimeout Nothing cmds = cmds
    addTimeout (Just secs) cmds = ["timeout",prettyString secs++"s"]++cmds
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
        forM_ (ioEnv ioargs) $ \(evar,epath) -> Sh.setenv (T.pack evar) (T.pack epath)
        let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
        stdout <- Sh.errExit False $ Sh.run (shFromFilePath $ head cmds) (map T.pack $ tail cmds)
        stderr <- if ioHidden ioargs then return (T.pack "hidden") else Sh.lastStderr
        exit <- Sh.lastExitCode
        evaluate $! force $! IOResult exit stdout stderr
      where
        addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
        addTimeout Nothing cmds = cmds
        addTimeout (Just secs) cmds = ["timeout",prettyString secs++"s"]++cmds
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
    forM_ (ioEnv ioargs) $ \(evar,epath) -> Sh.setenv (T.pack evar) (T.pack epath)
    let cmds = addEnv $ addTimeout (ioTimeout ioargs) $ addSandbox (ioSandbox ioargs) (name:args)
    a <- Sh.errExit False $ Sh.runHandle (shFromFilePath $ head cmds) (map T.pack $ tail cmds) handle
    return a
  where
    addEnv cmd = case ioCmd ioargs of { Nothing -> cmd; Just env -> env:cmd }
    addTimeout Nothing cmds = cmds
    addTimeout (Just secs) cmds = ["timeout",prettyString secs++"s"]++cmds
    addSandbox NoSandbox cmds = cmds
    addSandbox (Sandbox Nothing) cmds = ["cabal","exec","--"]++cmds
    addSandbox (Sandbox (Just cfg)) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds

shPipeWith :: (Show a,Read b,Typeable b) => IOArgs -> String -> [String] -> a -> Sh b
shPipeWith io n args x = shPipeWithType io n args x Proxy
    where
    shPipeWithType :: (Show a,Read b,Typeable b) => IOArgs -> String -> [String] -> a -> Proxy b -> Sh b
    shPipeWithType io n args x (_::Proxy b) = do
        let typeb = typeOf (error "shPipeWith"::b)
        let io' = io { ioStdin = Just $ T.pack $ show x }
        res <- orEitherSh $ shCommandWith io' n args
        case res of
            Left err -> error $ "error...\n" ++ prettyString err ++ "\n...on parsing result as type...\n" ++ show typeb
            Right res -> do
                let out = (T.unpack . resStdout) res
                case readMaybe out of
                    Nothing -> error $ "failed to parse result...\n" ++ show out ++ "\n...as type...\n" ++ show typeb ++ "\n" ++ prettyString res
                    Just y -> return y

shPipeWith_ :: (Show a) => IOArgs -> String -> [String] -> a -> Sh ()
shPipeWith_ io n args x = do
    let io' = io { ioStdin = Just $ T.pack $ show x }
    res <- orEitherSh $ shCommandWith io' n args
    case res of
        Left err -> error $ "error...\n" ++ prettyString err ++ "\n...on getting result"
        Right res -> do
            liftIO $ hPutStrLn stderr $ prettyString res

orEitherSh :: Sh a -> Sh (Either SomeException a)
orEitherSh m = catchAnySh (liftM Right m) (\err -> return $ Left err)

shWriteFile :: FilePath -> T.Text -> Sh ()
shWriteFile path ct = do
    Sh.mkdir_p $ shFromFilePath $ takeDirectory path
    Sh.writefile (shFromFilePath path) ct

shAppendFile :: FilePath -> T.Text -> Sh ()
shAppendFile path ct = do
    Sh.appendfile (shFromFilePath path) ct

shReadFile :: FilePath -> Sh T.Text
shReadFile path = do
    ct <- Sh.readfile (shFromFilePath path)
    return ct

shReadFile' :: FilePath -> Sh T.Text
shReadFile' path = do
    path' <- Sh.absPath (shFromFilePath path)
    ct <- liftIO $ T.readFile (shToFilePath path')
    return ct

shWriteFile' :: FilePath -> T.Text -> Sh ()
shWriteFile' path ct = do
    Sh.mkdir_p $ shFromFilePath $ takeDirectory path
    path' <- Sh.absPath (shFromFilePath path)
    liftIO $ T.writeFile (shToFilePath path') ct

shFromFilePath :: FilePath -> Sh.FilePath
shFromFilePath = Sh.fromText . T.pack

shToFilePath :: Sh.FilePath -> FilePath
shToFilePath = T.unpack . Sh.toTextIgnore

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
            forM_ froms $ \x -> shRecursive op (from </> makeRelative from x) (to </> makeRelative from x)       
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

catchAnySh :: Sh a -> (SomeException -> Sh a) -> Sh a
catchAnySh = catchSh

catchSh :: (Exception e) => Sh a -> (e -> Sh a) -> Sh a
catchSh f g = (evaluateM f) `Sh.catch_sh` \e ->
    if isSyncException e then g e else throw e
