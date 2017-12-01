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
import Control.Exception (evaluate)

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

import System.Timeout
import System.FilePath
import System.Exit
import System.Process
import System.Directory
import System.Environment

import Test.QuickCheck

import Text.Read

import GHC.Stack
import System.IO

import Shelly (Sh(..),catchany_sh)
import qualified Shelly as Sh

instance HaapMonad m => MonadIO (Haap p args db m) where
    {-# INLINE liftIO #-}
    liftIO io = runIOWith def io

data IOArgs = IOArgs
    { ioTimeout :: Maybe Int -- in seconds
    , ioSilent :: Bool -- run silently without printing to stderr or stdout
    , ioStdin :: Maybe Text -- input file or handle for processes
    , ioSandbox :: Maybe FilePath -- run inside a cabal sandbox (with given config file) or not; relative to the current path
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
    }

resOk :: IOResult -> Bool
resOk = (==0) . resExitCode

exitCode :: ExitCode -> Int
exitCode (ExitFailure i) = i
exitCode (ExitSuccess) = 0

instance Monoid IOResult where
    mempty = IOResult 0 Text.empty Text.empty
    mappend x y = IOResult
        (resExitCode x `min` resExitCode y)
        (resStdout x `Text.append` resStdout y)
        (resStdout x `Text.append` resStdout y)

instance Out IOResult where
    docPrec i x = doc x
    doc io =   text "Output:" $+$ text (Text.unpack $ resStdout io)
           $+$ text "Errors:" $+$ text (Text.unpack $ resStderr io)
           $+$ text "Exit Code:" <+> doc (resExitCode io)

defaultIOArgs :: IOArgs
defaultIOArgs = IOArgs (Just 60) False Nothing Nothing True [] Nothing False

hiddenIOArgs :: IOArgs
hiddenIOArgs = defaultIOArgs {ioHidden = True, ioSilent = True }

instance Default IOArgs where
    def = defaultIOArgs

runIOWithTimeout :: HaapMonad m => Int -> IO a -> Haap p args db m a
runIOWithTimeout timeout m = runIOWith (const args) m
    where
    args = def { ioTimeout = Just timeout }

runShWithTimeout :: HaapMonad m => Int -> Sh a -> Haap p args db m a
runShWithTimeout timeout m = runShWith (const args) m
    where
    args = def { ioTimeout = Just timeout }

runIOWith :: HaapMonad m => (args -> IOArgs) -> IO a -> Haap p args db m a
runIOWith getArgs io = do
    args <- Reader.reader getArgs
    runIOCore args io

runIOWith' :: (HaapMonad m,NFData a) => (args -> IOArgs) -> IO a -> Haap p args db m a
runIOWith' getArgs io = forceM $ runIOWith getArgs io

runIO :: HaapMonad m => IO a -> Haap p args db m a
runIO = runIOWith (const defaultIOArgs)

runIOExit :: HaapMonad m => IO () -> Haap p args db m ExitCode
runIOExit m = runIOWith (const defaultIOArgs) (catch (m >> exitSuccess) catchExit)
    where
    catchExit :: ExitCode -> IO ExitCode
    catchExit e = return e

ioExit :: IO () -> IO ExitCode
ioExit m = catch (m >> exitSuccess) catchExit
    where
    catchExit :: ExitCode -> IO ExitCode
    catchExit e = return e

runShWith :: HaapMonad m => (args -> IOArgs) -> Sh a -> Haap p args db m a
runShWith getArgs io = do
    args <- Reader.reader getArgs
    runShCore args io

runIOResultWith :: (args -> IOArgs) -> IO IOResult -> IO IOResult
runIOResultWith args m = orDoIO (\err -> return $ IOResult (-1) Text.empty (Text.pack $ pretty err)) m

runShIOResultWith :: HaapMonad m => (args -> IOArgs) -> Sh IOResult -> Haap p args db m IOResult
runShIOResultWith args m = orDo (\err -> return $ IOResult (-1) Text.empty (Text.pack $ pretty err)) (runShWith args m)

runShIOResult :: HaapMonad m => Sh IOResult -> Haap p args db m IOResult
runShIOResult m = orDo (\err -> return $ IOResult (-1) Text.empty (Text.pack $ pretty err)) (runSh m)

runSh :: HaapMonad m => Sh a -> Haap p args db m a
runSh = runShWith (const defaultIOArgs)

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
    addSandbox Nothing cmds = cmds
    addSandbox (Just cfg) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds
    
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
    addSandbox Nothing cmds = cmds
    addSandbox (Just cfg) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds
    handle h = do
        bs <- liftIO $ BS.hGetContents h
        liftIO $ BS.writeFile file bs

shCommand :: String -> [String] -> Sh IOResult
shCommand = shCommandWith defaultIOArgs

haapRetry :: HaapMonad m => Int -> Haap p args db m a -> Haap p args db m a
haapRetry 0 m = m
haapRetry i m = orDo (\e -> logError e >> haapRetry (pred i) m) m

shCommandWith :: IOArgs -> String -> [String] -> Sh IOResult
shCommandWith ioargs name args  = do
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
    addSandbox Nothing cmds = cmds
    addSandbox (Just cfg) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds

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
    addSandbox Nothing cmds = cmds
    addSandbox (Just cfg) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds

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
    addTimeout (Just secs) cmds = ["timeout",pretty secs++"s"]++cmds
--    addRedir cmds = if ioHidden ioargs then cmds ++ ["2>/dev/null"] else cmds
    addSandbox Nothing cmds = cmds
    addSandbox (Just cfg) cmds = ["cabal","--sandbox-config-file="++cfg,"exec","--"]++cmds
    addHiddenIO m = if ioHidden ioargs then Sh.catchany m (\err -> return $ mempty) else m

hiddenSh :: Sh a -> Sh a
hiddenSh m = Sh.catchany_sh (hide m) (const $ return $ error "result is hidden!")
    where
    hide = Sh.print_commands False . Sh.log_stderr_with (const $ return ()) . Sh.silently . Sh.tracing False

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

orErrorWritePage :: (HaapMonad m,Out a) => FilePath -> a -> Haap p args db m a -> Haap p args db m a
orErrorWritePage path def m = orDo go $ do
    x <- m
    ok <- orLogDefault False $ runIO $ doesFileExist path
    unless ok $ orLogDefault () $ runSh $ shWriteFile path $ pretty x
    return x
  where
    go e = do
        runIO $ writeFile path $ pretty e
        return def

haapLiftIO :: HaapMonad m => IO a -> Haap p args db m a
haapLiftIO io = Haap $ catch (liftIO io) (\(e::SomeException) -> throwError $ HaapIOException e)

runShCore :: HaapMonad m => IOArgs -> Sh a -> Haap p args db m a
runShCore args sh = case ioTimeout args of
    Nothing -> haapLiftIO io
    Just secs -> do
        mb <- haapLiftIO $ timeout (secs * 10^6) io
        case mb of
            Nothing -> throwError $ HaapTimeout callStack secs
            Just a -> return a
  where
    io = Sh.shelly $ silent $ Sh.escaping (ioEscaping args) sh
    silent = if ioHidden args
        then hiddenSh
        else if ioSilent args then Sh.silently else Sh.verbosely

runShCoreIO :: IOArgs -> Sh a -> IO a
runShCoreIO args sh = case ioTimeout args of
    Nothing -> io
    Just secs -> do
        mb <- timeout (secs * 10^6) io
        case mb of
            Nothing -> error $ pretty $ HaapTimeout callStack secs
            Just a -> return a
  where
    io = Sh.shelly $ silent $ Sh.escaping (ioEscaping args) sh
    silent = if ioSilent args then Sh.silently else Sh.verbosely

runIOCore :: HaapMonad m => IOArgs -> IO a -> Haap p args db m a
runIOCore args io = case ioTimeout args of
    Nothing -> haapLiftIO io
    Just secs -> do
        mb <- haapLiftIO $ timeout (secs * 10^6) io
        case mb of
            Nothing -> throwError $ HaapTimeout callStack secs
            Just a -> return a

runIO' :: (HaapMonad m,NFData a) => IO a -> Haap p args db m a
runIO' = runIOWith' (const defaultIOArgs)

orEither :: HaapMonad m => Haap p args db m a -> Haap p args db m (Either HaapException a)
orEither m = orDo (\e -> return $ Left e) (liftM Right m)

orLogEither :: HaapMonad m => Haap p args db m a -> Haap p args db m (Either HaapException a)
orLogEither m = orDo (\e -> logEvent (pretty e) >> return (Left e)) (liftM Right m)

orMaybe :: HaapMonad m => Haap p args db m a -> Haap p args db m (Maybe a)
orMaybe m = orDo (\e -> return Nothing) (liftM Just m)

orLogMaybe :: HaapMonad m => Haap p args db m a -> Haap p args db m (Maybe a)
orLogMaybe m = orDo (\e -> logEvent (pretty e) >> return Nothing) (liftM Just m)

orDo :: HaapMonad m => (HaapException -> Haap p args db m a) -> Haap p args db m a -> Haap p args db m a
orDo ex m = catchError m ex

orDoIO :: (SomeException -> IO a) -> IO a -> IO a
orDoIO ex m = catch m ex

orLogDefault :: HaapMonad m => a -> Haap p args db m a -> Haap p args db m a
orLogDefault a m = orDo (\e -> logEvent (pretty e) >> return a) m

orDefault :: HaapMonad m => a -> Haap p args db m a -> Haap p args db m a
orDefault a m = orDo (\e -> return a) m

orMaybeIO :: IO a -> IO (Maybe a)
orMaybeIO m = catch (liftM Just m) (\(err::SomeException) -> return Nothing)

orEitherSh :: Sh a -> Sh (Either SomeException a)
orEitherSh m = catchany_sh (liftM Right m) (\err -> return $ Left err)

orError :: HaapMonad m => Haap p args db m a -> Haap p args db m (Either a HaapException)
orError m = orDo (return . Right) (liftM Left m)

orDo' :: (HaapMonad m,NFData a) => (HaapException -> Haap p args db m a) -> Haap p args db m a -> Haap p args db m a
orDo' ex m = catchError (forceM m) ex

ignoreError :: HaapMonad m => Haap p args db m () -> Haap p args db m ()
ignoreError m = orDo (\e -> logEvent (pretty e)) m

addMessageToError :: HaapMonad m => String -> Haap p args db m a -> Haap p args db m a
addMessageToError msg m = orDo (\e -> throwError $ HaapException $ msg ++ pretty e) m

orLogError :: (IsString str,HaapMonad m) => Haap p args db m str -> Haap p args db m str
orLogError m = orDo (\e -> logEvent (pretty e) >> return (fromString $ pretty e)) m

forceHaap :: (NFData a,HaapMonad m) => a -> Haap p args db m a
forceHaap x = liftIO $ evaluate $ force x

forceM :: (Monad m,NFData a) => m a -> m a
forceM m = do
    x <- m
    return $! DeepSeq.force x

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
    isfromdir <- shDoesDirectoryExist from
    shMkDir $ takeDirectory to
    if isfromdir
        then do
            istodir <- shDoesDirectoryExist to
            unless (istodir) $ shMkDir to
            froms <- shLs from
            forM_ froms $ \x -> shRecursive op (from </> x) to       
        else op from to

equalPathIO :: FilePath -> FilePath -> IO Bool
equalPathIO x y = do
    x' <- canonicalizePath x
    y' <- canonicalizePath y
    return $ x' `equalFilePath` y'

equalPathSh :: FilePath -> FilePath -> Sh Bool
equalPathSh x y = do
    x' <- shCanonalize x
    y' <- shCanonalize y
    return $ equalFilePath x y

forAllIO :: Int -> Gen a -> (a -> IO b) -> IO [b]
forAllIO num gen f = do
    xs <- generate $ vectorOf num gen
    mapM f xs
        
        
        

