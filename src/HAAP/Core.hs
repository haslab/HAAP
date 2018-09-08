{-
HAAP: Haskell Automated Assessment Platform

This module provides the core components of HAAP.
HAAP follows a 'configuration-as-an-application' architecture, common in Haskell web frameworks such as Happstack.

An HAAP @Project@ defines the initial configuration, that includes the project’s name and its filelesystem paths, together with listings of student groups and tasks. Each task is comprised by a set of files that can have both local (templates or oracles used for assessment) and/or remote (student solutions) representations.

Scripting is done in the @Haap@ monad, that provides basic logging and error-handling capabilities.
An @Haap@ program |p| can be run (|runHaap project p|) as a batch script that generates assessment data for a given |project|.
-}

{-# LANGUAGE UndecidableInstances, TypeOperators, ConstraintKinds, StandaloneDeriving, FlexibleContexts, RankNTypes, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, TypeFamilyDependencies, DeriveGeneric, TemplateHaskell #-}
module HAAP.Core where

import HAAP.Utils

import Control.Monad
import Control.Monad.Trans.Compose
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.Morph
import Control.Monad.Trans.Control
import Control.Monad.Reader (MonadReader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer (MonadWriter(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Control.Monad.State (MonadState(..),StateT(..))
import qualified Control.Monad.State as State
import Control.Monad.RWS (RWST(..))
import qualified Control.Monad.RWS as RWS
--import Control.Monad.Except (MonadError(..),ExceptT(..))
--import qualified Control.Monad.Except as Except
--import Control.Monad.Catch
import Control.Monad.Signatures
import Control.DeepSeq
import Control.Exception.Safe
--import qualified Control.Monad.Except as E
import qualified Control.Monad.Catch as C

import qualified Data.Text as T
import Data.DList as DList
import Data.List as List
import Data.Typeable
import Data.Data
import Data.SafeCopy
import Data.Default

import GHC.Stack
import GHC.Generics

import System.Directory

import Data.Text.Prettyprint.Doc as PP

-- General static project information

data Project = Project
	{ projectName :: String
	, projectPath :: FilePath -- absolute path
    , projectTmpPath :: FilePath -- relative path for the project's temp directory
	, projectGroups :: [Group]
	, projectTasks :: [Task]
    }
  deriving (Data,Typeable,Eq,Show,Ord)

instance Default Project where
    def = defaultProject

defaultProject :: Project
defaultProject = Project "Project" "." "." [] []

data Group = Group
	{ groupId :: String
	, groupStudents :: [String]
    }
  deriving (Data,Typeable,Read,Show,Eq,Ord,Generic)

instance Pretty Group where
    pretty x = unsafeViaShow x

instance NFData Group

data HaapFileType
    = HaapFileType { isStudent :: Bool, isTemplate :: Bool, isInstructor :: Bool }
    
    -- = HaapTemplateFile -- student files with a given template
    -- -- | HaapBinaryFile -- student files without a given template
    -- | HaapLibraryFile  -- common libraries for both students and instructors
    -- | HaapOracleFile -- instructor code that students can't see and is not copied into the student's directory
    -- | HaapOracleTemplateFile -- instructor code that students can't see with a given template aand is copied into the student's directory
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

runHaap :: Project -> Haap IdentityT IO a -> IO (a,HaapLog)
runHaap p (Haap m) = do
    createDirectoryIfMissing True $ projectTmpPath p
    (a,(),w') <- runIdentityT $ RWS.runRWST m (p) ()
--  printLog w'
    return (a,w')

newtype Haap (t :: (* -> *) -> * -> *) (m :: * -> *) (x :: *) = Haap { unHaap :: RWST Project HaapLog () (t m) x }
  deriving (Applicative,Functor,Monad,MonadWriter HaapLog,MonadCatch,MonadThrow)

--instance (MonadTrans t) => MonadTrans (Haap t) where
--    lift = Haap . lift . lift . lift

--instance MonadTrans t => MonadTransControl (Haap t) where

--mapHaapDB :: HaapMonad m => Haap p args st1 m st2 -> (st2 -> Haap p args st1 m ()) -> Haap p args st2 m a -> Haap p args st1 m a
--mapHaapDB get put (Haap m) = Haap $ do
--    (p,r) <- Reader.ask
--    st2 <- unHaap $ get
--    (x,st2',w') <- lift $ RWS.runRWST m (p,r) st2
--    unHaap $ put st2'
--    Writer.tell w'
--    return x

--instance MFunctor (Haap t) where
--    hoist f (Haap m) = Haap $ RWS.mapRWST (Except.mapExceptT f) m

mapHaapMonad :: (t1 m (a,(),HaapLog) -> t2 n (b,(),HaapLog)) -> Haap t1 m a -> Haap t2 n b
mapHaapMonad f (Haap m) = Haap $ RWS.mapRWST f m
--    where
--    g m3 = do
--        (e,st) <- runStateT m3
--        
--        
--        (Either HaapException (b,HaapLog),st)
--        
--        (Either HaapException ((b,st),HaapLog),st)

--mapHaapMonad' :: Haap m a -> Haap n b
--mapHaapMonad' m = liftWith () m

--liftWith :: Monad m => (Run t -> m a) -> t m a
--type Run t = forall n b. Monad n => t n b -> n (StT t b)

--liftBaseWith :: (RunInBase m b -> b a) -> m a
--type RunInBase m b = forall a. m a -> b (StM m a)

--mapRWST :: (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
--mapExceptT :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b

--instance HaapStack t m => MonadThrow (Haap t m) where
--    throwM e = Haap $ throw e
--    
--instance HaapStack t m => MonadCatch (Haap t m) where
--    catch (Haap m) f = Haap $ do
--        (p) <- Reader.ask
--        s <- State.get
--        e <- lift $ lift $ E.runExceptT $ RWS.runRWST m (p) s
--        case e of
--            Left err -> unHaap $ f $ SomeException err
--            Right (x,s',w') -> do
--                State.put s'
--                Writer.tell w'
--                return x

data HaapException = HaapException T.Text
                   | HaapTimeout CallStack Int
                   | HaapIOException SomeException
  deriving (Typeable,Show,Generic)

type HaapLog = DList HaapEvent 
data HaapEvent = HaapEvent CallStack T.Text
  deriving (Typeable,Show)

getProject :: HaapStack t m => Haap t m (Project)
getProject = Haap $ ask

getProjectName :: HaapStack t m => Haap t m String
getProjectName = liftM projectName getProject

getProjectPath :: HaapStack t m => Haap t m String
getProjectPath = liftM projectPath getProject

getProjectTmpPath :: HaapStack t m => Haap t m String
getProjectTmpPath = liftM projectTmpPath getProject

getProjectGroups :: HaapStack t m => Haap t m [Group]
getProjectGroups = liftM projectGroups getProject

getProjectGroup :: HaapStack t m => String -> Haap t m (Maybe Group)
getProjectGroup gname = do
    gs <- getProjectGroups
    return $ List.find ((==gname) . groupId) gs

getProjectTasks :: HaapStack t m => Haap t m [Task]
getProjectTasks = liftM projectTasks getProject

getProjectTaskFiles :: HaapStack t m => Haap t m [HaapFile]
getProjectTaskFiles = liftM (concatMap taskFiles) getProjectTasks

type HaapMonad m = (Monad m,MonadCatch m,MonadThrow m)

-- | @MonadTrans@ equivalent
class (HaapMonad m,HaapMonad (t m)) => HaapStack t m where
    liftStack :: m a -> t m a

instance (HaapMonad m,HaapStack t m) => HaapStack (Haap t) m where
    liftStack = liftHaap . liftStack

instance {-# OVERLAPPABLE #-} (MonadTrans t,HaapMonad m,HaapMonad (t m)) => HaapStack t m where
    liftStack = lift

liftHaap :: Monad (t m) => t m a -> Haap t m a
liftHaap m = Haap $ lift m





