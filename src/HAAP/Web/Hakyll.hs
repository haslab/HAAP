{-# LANGUAGE OverloadedStrings #-}



module HAAP.Web.Hakyll
    ( module HAAP.Web.Hakyll
    , module Hakyll
    ) where
        
import HAAP.Core
import HAAP.IO

import System.FilePath
import System.Directory
import System.Environment
import System.Exit

import Control.Monad

import Data.Functor.Contravariant

import Hakyll

import Paths_HAAP

runHakyllWith :: Configuration -> Rules () -> Haap p args db ()
runHakyllWith cfg rules = ignoreError $ do
    copyDataFiles cfg
    let datarules = do
        matchDataTemplates
        matchDataCSSs
        matchDataJSs
        rules
    let build = runIOExit $ withArgs ["build"] $ hakyllWith cfg datarules
    let clean = runIOExit $ withArgs ["clean"] $ hakyllWith cfg datarules
    orDo (\e -> clean >> build) build
    return ()

copyDataFiles :: Configuration -> Haap p args db ()
copyDataFiles cfg = do
    datapath <- runIO $ getDataFileName ""
    xs <- runIO $ listDirectory datapath
    forM_ xs $ \x -> copyRecursive (datapath </> x) (providerDirectory cfg </> x)

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
    match (fromGlob ("templates" </> "*.markdown")) $ do
        route $ setExtension "html"
        compile $ pandocCompiler

matchDataCSSs :: Rules ()
matchDataCSSs = do
    match (fromGlob ("css" </> "*.css")) $ do
        route idRoute
        compile compressCssCompiler

instance Contravariant Context where
    contramap f (Context g) = Context $ \n ns x -> g n ns (fmap f x)

--loadAndApplyDataTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
--loadAndApplyDataTemplate path c i = do
--    path' <- fromDataFileName $ toFilePath path
--    loadAndApplyTemplate path' c i
--
--fromDataFileName :: FilePath -> Compiler Identifier
--fromDataFileName path = liftM fromFilePath $ unsafeCompiler $ getDataFileName path



