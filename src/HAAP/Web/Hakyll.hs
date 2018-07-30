{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Hakyll@ plugin (<https://hackage.haskell.org/package/hakyll>) for static web-page generation.
-}

{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleContexts, EmptyDataDecls, FlexibleInstances, TypeFamilies, OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving, MultiParamTypeClasses, Rank2Types #-}

module HAAP.Web.Hakyll
    ( module HAAP.Web.Hakyll
    , module Hakyll
    ) where
        
import HAAP.Core
import HAAP.IO
import HAAP.Pretty
import HAAP.Plugin
import HAAP.Shelly
import HAAP.Utils

import System.FilePath
import System.Directory
import System.Environment
import System.Exit

import Control.Applicative
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Identity
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Writer (MonadWriter(..),WriterT(..))
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import qualified Control.Monad.RWS as RWS
import Control.Monad.RWS (RWST(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Catch (MonadCatch,MonadThrow)
import Control.Monad.Trans

import Data.Functor.Contravariant
import Data.Default
import Data.Proxy

import Hakyll

import Paths_HAAP

-- * Hakyll plugin

data Hakyll

data HakyllArgs = HakyllArgs
    { hakyllCfg :: Configuration
    , hakyllClean :: Bool
    , hakyllCopy :: Bool
    , hakyllP :: HakyllP
    }

instance Default HakyllArgs where
    def = defaultHakyllArgs

defaultHakyllArgs :: HakyllArgs
defaultHakyllArgs = HakyllArgs def True True def

instance HaapPlugin Hakyll where
    type PluginI Hakyll = HakyllArgs
    type PluginO Hakyll = ()
    type PluginT Hakyll = HakyllT
    type PluginK Hakyll t m = (MonadIO m)
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- runHaapHakyllT args m
        return (x,())

useHakyll :: (HaapStack t m,PluginK Hakyll t m) => (PluginI Hakyll) -> Haap (PluginT Hakyll :..: t) m a -> Haap t m a
useHakyll args = usePlugin_ (return args)

instance Monoid (Rules ()) where
    mempty = return ()
    mappend x y = x >> y

newtype HakyllT m a = HakyllT { unHakyllT :: RWST HakyllArgs (Rules ()) HakyllP m a }
  deriving (Functor,Applicative,Monad,MFunctor,MonadTrans,MonadIO,MonadCatch,MonadThrow,MonadReader HakyllArgs,MonadState HakyllP,MonadWriter (Rules ()))

instance HaapMonad m => HaapStack HakyllT m where
    liftStack = lift

instance HaapMonad m => HasPlugin Hakyll HakyllT m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT HakyllT m (t2 m)) => HasPlugin Hakyll (ComposeT HakyllT t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m

morphHakyllT :: (forall b . m b -> n b) -> HakyllT m a -> HakyllT n a
morphHakyllT f (HakyllT m) = HakyllT $ RWS.mapRWST f m

hakyllRules :: HasPlugin Hakyll t m => Rules () -> Haap t m ()
hakyllRules r = liftHaap $ liftPluginProxy (Proxy::Proxy Hakyll) $ Writer.tell r

runHaapHakyllT :: (HaapStack t m,MonadIO m) => PluginI Hakyll -> Haap (HakyllT :..: t) m a -> Haap t m a
runHaapHakyllT args m = do
    let go :: (MonadIO m,HaapStack t m) => forall b . (HakyllT :..: t) m b -> t m b
        go (ComposeT (HakyllT m)) = do
            (e,hp,rules) <- RWS.runRWST m (args) (hakyllP args)
            let datarules = do
                matchDataTemplates
                matchDataCSSs
                matchDataJSs
                rules
            let build = liftStack $ liftIO $ withArgs ["build"] $ hakyllWithExitCode (hakyllCfg args) datarules
            let clean = liftStack $ liftIO $ withArgs ["clean"] $ hakyllWithExitCode (hakyllCfg args) datarules
            if (hakyllClean args)
                then clean >> build
                else build
            --    else catch
            --        (build >>= \e -> case e of { ExitFailure _ -> clean >> build; otherwise -> return e })
            --        (\(err::SomeException) -> clean >> build)
            return e
    when (hakyllCopy args) $ copyDataFiles (hakyllCfg args)
    e <- mapHaapMonad go m
    return e
    

copyDataFiles :: (MonadIO m,HaapStack t m) => Configuration -> Haap t m ()
copyDataFiles cfg = do
    datapath <- runBaseIO' $ getDataFileName ""
    xs <- runBaseIO' $ listDirectory datapath
    runBaseSh $ forM_ xs $ \x -> shCpRecursive (datapath </> x) (providerDirectory cfg </> x)

matchDataJSs :: Rules ()
matchDataJSs = do
    match (fromGlob ("js" </> "*.js")) $ do
        route idRoute
        compile copyFileCompiler

matchDataTemplates :: Rules ()
matchDataTemplates = do
    match (fromGlob ("templates" </> "*.html")) $ do
        route idRoute
        compile templateBodyCompiler
    match (fromGlob ("templates" </> "*.php")) $ do
        route idRoute
        compile templateBodyCompiler
    match (fromGlob ("templates" </> "*.markdown")) $ do
        route $ setExtension "html"
        compile $ pandocCompiler

matchDataCSSs :: Rules ()
matchDataCSSs = do
    match (fromGlob ("css" </> "*.css")) $ do
        route idRoute
        compile compressCssCompiler

orErrorHakyllPage :: (HasPlugin Hakyll t m,Out a) => FilePath -> a -> Haap t m a -> Haap t m a
orErrorHakyllPage page def m = orDo go m
  where
    go e = do
        hp <- getHakyllP
        hakyllRules $ create [fromFilePath page] $ do
            route $ idRoute `composeRoutes` (funRoute $ hakyllRoute hp)
            compile $ makeItem (pretty e::String) >>= hakyllCompile hp
        return def

orErrorHakyllPage' :: (MonadIO m,HaapStack t m,Out a) => HakyllArgs -> FilePath -> a -> Haap t m a -> Haap t m a
orErrorHakyllPage' hakyllargs page def m = orDo go m
  where
    go e = useHakyll hakyllargs $ do
        hp <- getHakyllP
        hakyllRules $ create [fromFilePath page] $ do
            route $ idRoute `composeRoutes` (funRoute $ hakyllRoute hp)
            compile $ makeItem (pretty e::String) >>= hakyllCompile hp
        return def

orErrorHakyllPageWith :: (MonadIO m,HaapStack t m,Out a) => (forall a . Haap (HakyllT :..: t) m a -> Haap t m a) -> FilePath -> a -> Haap t m a -> Haap t m a
orErrorHakyllPageWith runHakyll page def m = orDo go m
  where
    go e = runHakyll $ do
        hp <- getHakyllP
        hakyllRules $ create [fromFilePath page] $ do
            route $ idRoute `composeRoutes` (funRoute $ hakyllRoute hp)
            compile $ makeItem (pretty e::String) >>= hakyllCompile hp
        return def

getHakyllP :: (HasPlugin Hakyll t m) => Haap t m HakyllP
getHakyllP = liftHaap $ liftPluginProxy (Proxy::Proxy Hakyll) $ State.get

getHakyllArgs :: (HasPlugin Hakyll t m) => Haap t m HakyllArgs
getHakyllArgs = liftHaap $ liftPluginProxy (Proxy::Proxy Hakyll) $ Reader.ask

-- * Hakyll Utilities

loadAndApplyHTMLTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
loadAndApplyHTMLTemplate iden ctx item = do
    i <- loadAndApplyTemplate iden ctx item
    return i

relativeRoute :: FilePath -> Routes
relativeRoute prefix = customRoute $ \iden -> makeRelative (prefix) (toFilePath iden)

liftCompiler :: (String -> String) -> Item String -> Compiler (Item String)
liftCompiler f i = return $ fmap f i

funRoute :: (FilePath -> FilePath) -> Routes
funRoute f = customRoute (f . toFilePath)

instance Contravariant Context where
    contramap f (Context g) = Context $ \n ns x -> g n ns (fmap f x)

----loadAndApplyDataTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
----loadAndApplyDataTemplate path c i = do
----    path' <- fromDataFileName $ toFilePath path
----    loadAndApplyTemplate path' c i
----
----fromDataFileName :: FilePath -> Compiler Identifier
----fromDataFileName path = liftM fromFilePath $ unsafeCompiler $ getDataFileName path

loadCopyFile :: Item CopyFile -> Compiler (Item String)
loadCopyFile = load . fromFilePath . (\(CopyFile f) -> f) . itemBody

dataRoute :: FilePath -> Routes
dataRoute datapath = customRoute (\iden -> makeRelative datapath $ toFilePath iden)

-- * Hakyll pre-processor

data HakyllP = HakyllP { hakyllRoute :: FilePath -> FilePath, hakyllCompile :: Item String -> Compiler (Item String) }

instance Monoid HakyllP where
    mempty = defaultHakyllP
    mappend x y = HakyllP (hakyllRoute y . hakyllRoute x) (hakyllCompile x >=> hakyllCompile y)

instance Default HakyllP where
    def = defaultHakyllP
    
defaultHakyllP :: HakyllP
defaultHakyllP = HakyllP id return

readHakyllP :: HasPlugin Hakyll t m => Haap t m HakyllP
readHakyllP = liftHaap $ liftPluginProxy (Proxy::Proxy Hakyll) $ State.get

writeHakyllP :: HasPlugin Hakyll t m => HakyllP -> Haap t m ()
writeHakyllP hp = liftHaap $ liftPluginProxy (Proxy::Proxy Hakyll) $ State.put hp

withHakyllP :: HasPlugin Hakyll t m => HakyllP -> Haap t m a -> Haap t m a
withHakyllP newhp m = do
    oldhp <- readHakyllP
    writeHakyllP newhp
    x <- m
    writeHakyllP oldhp
    return x







