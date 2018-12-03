{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Haddock@ plugin that invokes the _haddock_ tool (<https://hackage.haskell.org/package/haddock>) to generate documentation of Haskell code.

-}

{-# LANGUAGE TypeOperators, EmptyDataDecls, GeneralizedNewtypeDeriving, TypeFamilies, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}

module HAAP.Doc.Haddock where

import HAAP.Core
import HAAP.IO
import HAAP.Plugin
import HAAP.Web.Hakyll
import HAAP.Web.HTML.TagSoup
import HAAP.Utils
import HAAP.Code.Haskell
import HAAP.Shelly

import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Exts.Pretty as Hs

import Data.Default
import qualified Data.Text as Text
import Data.Traversable
import Data.List

import Control.Monad.Reader as Reader
import Control.Monad.State as State
--import Control.Monad.Catch

import System.FilePath

import Data.Proxy


data Haddock

instance HaapPlugin Haddock where
    type PluginI Haddock = HaddockArgs
    type PluginT Haddock = StateT HaddockArgs
    type PluginO Haddock = ()
    type PluginK Haddock t m = (MonadIO m)

    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip State.evalStateT args . getComposeT) m
        return (x,())

data HaddockArgs = HaddockArgs
    { haddockSandbox :: Sandbox
    , haddockTitle :: String
    , haddockArgs :: [String]
    , haddockPath :: FilePath -- path relative to the project where to execute the haddock command
    , haddockFiles :: [FilePath] -- relative to the path where haddock is executed
    , haddockExtraDirs :: [FilePath] -- additional files to be copied to the haddock html folder
    , haddockHtmlPath :: FilePath -- relative to the project path
    }

instance HaapMonad m => HasPlugin Haddock (StateT HaddockArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin Haddock (ComposeT (StateT HaddockArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m

useAndRunHaddock :: (MonadIO m,HasPlugin Hakyll t m) => HaddockArgs -> Haap t m FilePath
useAndRunHaddock args = usePlugin_ (return args) $ runHaddock

runHaddock :: (MonadIO m,HasPlugin Haddock t m,HasPlugin Hakyll t m) => Haap t m FilePath
runHaddock = do
    h <- liftHaap $ liftPluginProxy (Proxy::Proxy Haddock) $ State.get
    let files = haddockFiles h
    files' <- forM files $ \f -> do
        mb <- orEither $ parseModuleFileName $ haddockPath h </> f
        let isMain = either (const False) (=="Main") mb
        --liftIO $ putStrLn $ "module " ++ show mb
        --liftIO $ putStrLn $ "haddock " ++ show f ++ " " ++ show isMain
        return (f,isMain)
    let (mains,others) = partition (snd) files'
    
    let haddock = map fst others
    mains <- forM (nub $ map fst mains) $ \f -> do
        return [f]
    let haddocks = haddock : mains
    runHaddocks haddocks

runHaddocks :: (MonadIO m,HasPlugin Haddock t m,HasPlugin Hakyll t m) => [[FilePath]] -> Haap t m FilePath
runHaddocks allfiles = do
    h <- liftHaap $ liftPluginProxy (Proxy::Proxy Haddock) $ State.get
    hp <- getHakyllP
    tmp <- getProjectTmpPath
    let ioArgs = def { ioSandbox = mapSandboxCfg (dirToRoot (haddockPath h) </>) (haddockSandbox h) }
    let extras = haddockArgs h
    
    let html = dirToRoot (haddockPath h) </> tmp </> haddockHtmlPath h
    let indexhtml = addExtension (haddockHtmlPath h) "html"
    
    allres <- forM (zip [0..] allfiles) $ \(i,files) -> do
        orErrorWritePage (addExtension (tmp </> show i) "html") mempty $ runBaseSh $ do
        shCd $ haddockPath h
        shCommandWith ioArgs "haddock" (extras++["-h","-o",html </> show i]++files)
    forM_ (zip [0..] allres) $ \(i,_) -> do
        let dirs = map (haddockPath h </>) (haddockExtraDirs h)
        hakyllFocus ((tmp </> haddockHtmlPath h):dirs) $ hakyllRules $ do
            forM dirs $ \dir -> do
                match (fromGlob (dir </> "**")) $ do
                    route $ relativeRoute (haddockPath h) `composeRoutes` addToRoute (haddockHtmlPath h </> show i)
                    compile copyFileCompiler
            -- copy the haddock generated documentation
            match (fromGlob $ (tmp </> haddockHtmlPath h </> show i) </> "*.html") $ do
                route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
                compile $ getResourceString >>= liftCompiler (asTagSoupHTML $ tagSoupChangeLinkUrls $ hakyllRoute hp) >>= hakyllCompile hp
            let auxFiles = fromGlob (tmp </> haddockHtmlPath h </> show i </> "*.js")
                           .||. fromGlob (tmp </> haddockHtmlPath h </> show i </> "*.png")
                           .||. fromGlob (tmp </> haddockHtmlPath h </> show i </> "*.gif")
                           .||. fromGlob (tmp </> haddockHtmlPath h </> show i </> "*.css")
            match auxFiles $ do
                route   $ relativeRoute tmp
                compile $ copyFileCompiler
        -- generate a documentation page with the haddock report and a link to the documentation
    hakyllFocus ["templates"] $ hakyllRules $ do
        let mkName (_,files,_) = sepByStr " " files
        create [fromFilePath indexhtml] $ do
            route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
            compile $ do
                let haddockCtx = field "name" (return . mkName . itemBody)
                       `mappend` field "stdout" (return . Text.unpack . resStdout . thr3 . itemBody)
                       `mappend` field "stderr" (return . Text.unpack . resStderr . thr3 . itemBody)
                       `mappend` field "link" (\item -> return $ hakyllRoute hp $ addExtension (takeFileName (haddockHtmlPath h) </> (show $ fst3 $ itemBody item) </> "index") "html")
                let docCtx = constField "title" (haddockTitle h)
                           `mappend` constField "projectpath" (dirToRoot $ haddockPath h)
                           `mappend` listField "haddocks" haddockCtx (mapM makeItem $ zip3 [0..] allfiles allres)      
                makeItem "" >>= loadAndApplyHTMLTemplate "templates/doc.html" docCtx >>= hakyllCompile hp
    return $ hakyllRoute hp indexhtml
        