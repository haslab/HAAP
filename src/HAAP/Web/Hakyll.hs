{-# LANGUAGE FlexibleInstances, TypeFamilies, OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}



module HAAP.Web.Hakyll
    ( module HAAP.Web.Hakyll
    , module Hakyll
    ) where
        
import HAAP.Core
import HAAP.IO
import HAAP.Pretty

import System.FilePath
import System.Directory
import System.Environment
import System.Exit

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Writer (MonadWriter(..),WriterT(..))
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.Except as Except
import qualified Control.Monad.RWS as RWS
import Control.Monad.Catch (MonadCatch,MonadThrow)
import Control.Monad.Trans

import Data.Functor.Contravariant

import Hakyll

import Paths_HAAP

instance Monoid (Rules ()) where
    mempty = return ()
    mappend x y = x >> y

newtype Hakyll a = Hakyll { unHakyll :: WriterT (Rules ()) IO a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadCatch,MonadThrow,MonadWriter (Rules ()))

instance HaapMonad Hakyll where
    type HaapMonadArgs Hakyll = (Configuration,Bool)
    runHaapMonadWith = runHakyllWith

loadAndApplyHTMLTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
loadAndApplyHTMLTemplate iden ctx item = do
    i <- loadAndApplyTemplate iden ctx item
    return i

relativeRoute :: FilePath -> Routes
relativeRoute prefix = customRoute $ \iden -> makeRelative prefix (toFilePath iden)

hakyllRules :: Rules () -> Haap p args db Hakyll ()
hakyllRules r = Haap $ lift $ lift $ Writer.tell r

runHakyllWith :: (args -> HaapMonadArgs Hakyll) -> Haap p args db Hakyll a -> Haap p args db IO a
runHakyllWith getCfg (Haap mrules) = do
    (cfg,doClean) <- Reader.reader getCfg
    let g (Hakyll m) = do
        (e,rules) <- Writer.runWriterT m
        let datarules = do
            matchDataTemplates
            matchDataCSSs
            matchDataJSs
            rules
        let build = withArgs ["build"] $ hakyllWithExitCode cfg datarules
        let clean = withArgs ["clean"] $ hakyllWithExitCode cfg datarules
        if doClean
            then clean >> build
            else catch
                (build >>= \e -> case e of { ExitFailure _ -> clean >> build; otherwise -> return e })
                (\(err::SomeException) -> clean >> build)
        return e
    copyDataFiles cfg
    Haap $ RWS.mapRWST (Except.mapExceptT g) mrules

copyDataFiles :: HaapMonad m => Configuration -> Haap p args db m ()
copyDataFiles cfg = do
    datapath <- runIO $ getDataFileName ""
    xs <- runIO $ listDirectory datapath
    runSh $ forM_ xs $ \x -> shCpRecursive (datapath </> x) (providerDirectory cfg </> x)

--dataRoute :: FilePath -> Routes
--dataRoute datapath = customRoute (\iden -> makeRelative datapath $ toFilePath iden)

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

data HakyllP = HakyllP { hakyllRoute :: Routes, hakyllCompile :: Item String -> Compiler (Item String) }

defaultHakyllP :: HakyllP
defaultHakyllP = HakyllP idRoute return

instance Contravariant Context where
    contramap f (Context g) = Context $ \n ns x -> g n ns (fmap f x)

--loadAndApplyDataTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
--loadAndApplyDataTemplate path c i = do
--    path' <- fromDataFileName $ toFilePath path
--    loadAndApplyTemplate path' c i
--
--fromDataFileName :: FilePath -> Compiler Identifier
--fromDataFileName path = liftM fromFilePath $ unsafeCompiler $ getDataFileName path

orErrorHakyllPage :: (Out a) => HakyllP -> FilePath -> a -> Haap p args db Hakyll a -> Haap p args db Hakyll a
orErrorHakyllPage hp page def m = orDo go m
  where
    go e = do
        hakyllRules $ create [fromFilePath page] $ do
            route $ idRoute `composeRoutes` (hakyllRoute hp)
            compile $ makeItem (show e::String) >>= hakyllCompile hp
        return def

loadCopyFile :: Item CopyFile -> Compiler (Item String)
loadCopyFile = load . fromFilePath . (\(CopyFile f) -> f) . itemBody
