{-# LANGUAGE FlexibleContexts, RankNTypes, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, TypeFamilyDependencies, DeriveGeneric, TemplateHaskell #-}
module HAAP.Core where

import HAAP.Utils

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader (MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer as Writer
import Control.Monad.State (MonadState(..))
import qualified Control.Monad.State as State
import Control.Monad.RWS (RWST(..))
import qualified Control.Monad.RWS as RWS
import Control.Monad.Except (MonadError(..),ExceptT(..))
import qualified Control.Monad.Except as Except
import Control.Monad.Catch
import Control.Monad.Signatures
import Control.DeepSeq

import Data.DList as DList
import Data.Typeable
import Data.Data
import Data.SafeCopy

import GHC.Stack
import GHC.Generics

import System.Directory

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

-- General static project information

data Project p = Project
	{ projectName :: String
	, projectPath :: FilePath -- absolute path
    , projectTmpPath :: FilePath -- relative path for the project's temp directory
	, projectGroups :: [Group]
	, projectTasks :: [Task]
    }
  deriving (Data,Typeable,Eq,Show,Ord)

data Group = Group
	{ groupId :: String
	, groupStudents :: [String]
    }
  deriving (Data,Typeable,Read,Show,Eq,Ord,Generic)

instance Out Group where
    docPrec i x = doc x
    doc x = text $ show x

instance NFData Group

data HaapFileType
    = HaapTemplateFile -- student files with a given template
    | HaapLibraryFile  -- common libraries for both students and instructors
    | HaapOracleFile -- instructor code that students can't see
  deriving (Data,Typeable,Eq,Show,Ord)

data HaapFile = HaapFile
    { haapLocalFile :: FilePath -- file in the project folder (the contents can be a template)
    , haapRemoteFile :: FilePath -- file in the group folder (the name itself is a template)
    , haapFileType :: HaapFileType
    }
  deriving (Data,Typeable,Eq,Show,Ord)

data Task = Task
	{ taskName :: String
	, taskFiles :: [HaapFile]  
    }
  deriving (Data,Typeable,Eq,Show,Ord)

$(deriveSafeCopy 0 'base ''Group)
$(deriveSafeCopy 0 'base ''HaapFileType)
$(deriveSafeCopy 0 'base ''HaapFile)
$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''Project)

runHaap :: Project p -> args -> Haap p args () IO a -> IO a
runHaap p args (Haap m) = do
    createDirectoryIfMissing True $ projectTmpPath p
    e <- Except.runExceptT $ RWS.runRWST m (p,args) ()
    case e of
        Left e -> error $ "Haap Error: " ++ pretty e
        Right (a,(),w') -> do
            printLog w'
            return a

newtype Haap p args db m x = Haap { unHaap :: RWST (Project p,args) HaapLog db (ExceptT HaapException m) x }
  deriving (Applicative,Functor,Monad,MonadWriter HaapLog)

instance MonadTrans (Haap p args db) where
    lift = Haap . lift . lift

instance Monad m => MonadReader args (Haap p args db m) where
    ask = Haap $ liftM snd ask
    local f (Haap m) = Haap $ local (mapSnd f) m
    reader f = Haap $ reader (f . snd)
    
haapWithReader :: HaapMonad m => (args -> args') -> Haap p args' db m a -> Haap p args db m a
haapWithReader f (Haap m) = Haap $ do
    (p,r) <- Reader.ask
    s <- State.get
    (x,s',w') <- lift $ RWS.runRWST m (p,f r) s
    State.put s'
    Writer.tell w'
    return x

mapHaapDB :: HaapMonad m => Haap p args st1 m st2 -> (st2 -> Haap p args st1 m ()) -> Haap p args st2 m a -> Haap p args st1 m a
mapHaapDB get put (Haap m) = Haap $ do
    (p,r) <- Reader.ask
    st2 <- unHaap $ get
    (x,st2',w') <- lift $ RWS.runRWST m (p,r) st2
    unHaap $ put st2'
    Writer.tell w'
    return x

instance HaapMonad m => MonadError HaapException (Haap p args db m) where
    {-# INLINE throwError #-}
    throwError e = Haap $ throwError e
    {-# INLINE catchError #-}
    catchError (Haap m) f = Haap $ do
        (p,r) <- Reader.ask
        s <- State.get
        e <- lift $ lift $ Except.runExceptT $ RWS.runRWST m (p,r) s
        case e of
            Left err -> unHaap $ f err
            Right (x,s',w') -> do
                State.put s'
                Writer.tell w'
                return x

getDB :: HaapMonad m => Haap p args db m db
getDB = Haap State.get

putDB :: HaapMonad m => db -> Haap p args db m ()
putDB db = Haap $ State.put db

data HaapException = HaapException String
                   | HaapTimeout CallStack Int
                   | HaapIOException SomeException
  deriving (Typeable,Show,Generic)
  
instance Out HaapException where
    docPrec i x = doc x
    doc (HaapException str) = text str
    doc (HaapTimeout stack i) = text "timed out after" <+> int i <+> text "seconds"
    doc (HaapIOException e) = text (displayException e)
  
instance Exception HaapException where
    displayException e = pretty e

type HaapLog = DList HaapEvent 
data HaapEvent = HaapEvent CallStack String
  deriving (Typeable,Show)
  
instance Out HaapEvent where
    docPrec i x = doc x
    doc (HaapEvent c s) = text (prettyCallStack c) <> char ':' $+$ nest 4 (text s)

printLog :: HaapLog -> IO ()
printLog l = forM_ l $ \e -> putStrLn $ pretty e

getProject :: HaapMonad m => Haap p args db m (Project p)
getProject = Haap $ liftM fst ask

getProjectName :: HaapMonad m => Haap p args db m String
getProjectName = liftM projectName getProject

getProjectPath :: HaapMonad m => Haap p args db m String
getProjectPath = liftM projectPath getProject

getProjectTmpPath :: HaapMonad m => Haap p args db m String
getProjectTmpPath = liftM projectTmpPath getProject

getProjectGroups :: HaapMonad m => Haap p args db m [Group]
getProjectGroups = liftM projectGroups getProject

getProjectTasks :: HaapMonad m => Haap p args db m [Task]
getProjectTasks = liftM projectTasks getProject

getProjectTaskFiles :: HaapMonad m => Haap p args db m [HaapFile]
getProjectTaskFiles = liftM (concatMap taskFiles) getProjectTasks

class (MonadIO m,Monad m,MonadCatch m,MonadThrow m) => HaapMonad m where
    type HaapMonadArgs m = r | r -> m
    runHaapMonadWith :: (args -> HaapMonadArgs m) -> Haap p args db m a -> Haap p args db IO a

instance HaapMonad IO where
    type HaapMonadArgs IO = ()
    runHaapMonadWith getArgs m = m




