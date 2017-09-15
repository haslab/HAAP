{-# LANGUAGE FlexibleContexts, RankNTypes, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, TypeFamilyDependencies, DeriveGeneric #-}
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

import Data.DList as DList
import Data.Typeable
import Data.Data

import GHC.Stack
import GHC.Generics

-- General static project information

data Project p = Project
	{ projectName :: String
	, projectPath :: FilePath -- absolute path
	, projectGroups :: [Group]
	, projectTasks :: [Task]
    }
  deriving (Data,Typeable,Read,Show)

runHaap :: Project p -> args -> Haap p args () a -> IO a
runHaap p args (Haap m) = do
    e <- Except.runExceptT $ RWS.runRWST m (p,args) ()
    case e of
        Left e -> error $ "Haap Error: " ++ show e
        Right (a,(),w') -> do
            printLog w'
            return a

data Group = Group
	{ groupId :: String
	, groupStudents :: [String]
    }
  deriving (Data,Typeable,Read,Show)

data Task = Task
	{ taskName :: String
	, taskPath :: FilePath -- relative path
	, taskTemplate :: [FilePath]  -- student files with a given template
	, taskOracle :: [FilePath]    -- instructor solutions or hidden files in the students directory
	, taskLibrary :: [FilePath]   -- common libraries for both students and instructors
    }
  deriving (Data,Typeable,Read,Show)

newtype Haap p args db x = Haap { unHaap :: RWST (Project p,args) HaapLog db (ExceptT HaapException IO) x }
  deriving (Applicative,Functor,Monad,MonadWriter HaapLog)

instance MonadReader args (Haap p args db) where
    ask = Haap $ liftM snd ask
    local f (Haap m) = Haap $ local (mapSnd f) m
    reader f = Haap $ reader (f . snd)
    
haapWithReader :: (args -> args') -> Haap p args' db a -> Haap p args db a
haapWithReader f (Haap m) = Haap $ do
    (p,r) <- Reader.ask
    s <- State.get
    (x,s',w') <- lift $ RWS.runRWST m (p,f r) s
    State.put s'
    Writer.tell w'
    return x

mapHaapDB :: Haap p args st1 st2 -> (st2 -> Haap p args st1 ()) -> Haap p args st2 a -> Haap p args st1 a
mapHaapDB get put (Haap m) = Haap $ do
    (p,r) <- Reader.ask
    st2 <- unHaap $ get
    (x,st2',w') <- lift $ RWS.runRWST m (p,r) st2
    unHaap $ put st2'
    Writer.tell w'
    return x

instance MonadError HaapException (Haap p args db) where
    {-# INLINE throwError #-}
    throwError e = Haap $ throwError e
    {-# INLINE catchError #-}
    catchError (Haap m) f = Haap $ do
        (p,r) <- Reader.ask
        s <- State.get
        e <- liftIO $ Except.runExceptT $ RWS.runRWST m (p,r) s
        case e of
            Left err -> unHaap $ f err
            Right (x,s',w') -> do
                State.put s'
                Writer.tell w'
                return x

getDB :: Haap p args db db
getDB = Haap State.get

putDB :: db -> Haap p args db ()
putDB db = Haap $ State.put db

data HaapException = HaapException String
                   | HaapTimeout CallStack Int
                   | HaapIOException SomeException
  deriving (Typeable,Show,Generic)
  
instance Exception HaapException

type HaapLog = DList HaapEvent 
data HaapEvent = HaapEvent CallStack String
  deriving (Typeable,Show)

printLog :: HaapLog -> IO ()
printLog l = forM_ l $ \e -> putStrLn $ show e

getProject :: Haap p args db (Project p)
getProject = Haap $ liftM fst ask

getProjectName = liftM projectPath getProject
getProjectPath = liftM projectPath getProject
getProjectGroups = liftM projectGroups getProject
getProjectTasks = liftM projectTasks getProject



