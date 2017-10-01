{-# LANGUAGE OverloadedStrings #-}

module HAAP.Doc.Haddock where

import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Web.HTML.TagSoup
import HAAP.Utils
import HAAP.Code.Haskell

import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Exts.Pretty as Hs

import Data.Default
import qualified Data.Text as Text
import Data.Traversable
import Data.List

import qualified Control.Monad.Reader as Reader

import System.FilePath

data HaddockArgs = HaddockArgs
    { haddockSandbox :: Maybe FilePath
    , haddockTitle :: String
    , haddockArgs :: [String]
    , haddockPath :: FilePath -- path relative to the project where to execute the haddock command
    , haddockFiles :: [FilePath] -- relative to the path where haddock is executed
    , haddockHtmlPath :: FilePath -- relative to the project path
    }

runHaddock :: HakyllP -> HaddockArgs -> Haap p args db Hakyll [FilePath]
runHaddock hp h = do
    let files = haddockFiles h
    files' <- forM files $ \f -> do
        mb <- orEither $ parseModuleFileName $ haddockPath h </> f
        let isMain = either (const False) (=="Main") mb
        runIO $ putStrLn $ "module " ++ show mb
        runIO $ putStrLn $ "haddock " ++ show f ++ " " ++ show isMain
        return (f,isMain)
    let (mains,others) = partition (snd) files'
    
    haddockpath <- runHaddock' (map fst others) hp h
    mainpaths <- forM (zip [1..] $ nub $ map fst mains) $ \(i,f) -> do
        let h' = h { haddockHtmlPath = haddockHtmlPath h ++ show i }
        runHaddock' [f] hp h'
    return $ haddockpath : mainpaths

runHaddock' :: [FilePath] -> HakyllP -> HaddockArgs -> Haap p args db Hakyll FilePath
runHaddock' files hp h = do
    tmp <- getProjectTmpPath
    let ioArgs = def { ioSandbox = fmap (dirToRoot (haddockPath h) </>) (haddockSandbox h) }
    let extras = haddockArgs h
    
    let html = dirToRoot (haddockPath h) </> tmp </> haddockHtmlPath h
    let indexhtml = addExtension (haddockHtmlPath h) "html"
    res <- orErrorWritePage (tmp </> indexhtml) mempty $ runSh $ do
        shCd $ haddockPath h
        shCommandWith ioArgs "haddock" (extras++["-h","-o",html]++files)
--    runIO $ putStrLn $ show $ resStderr res
--    runIO $ putStrLn $ show $ resStdout res
    hakyllRules $ do
        -- copy the haddock generated documentation
        match (fromGlob $ (tmp </> haddockHtmlPath h) </> "*.html") $ do
            route   $ relativeRoute tmp `composeRoutes` funRoute (hakyllRoute hp)
            compile $ getResourceString >>= liftCompiler (asTagSoupHTML $ tagSoupChangeLinkUrls $ hakyllRoute hp) >>= hakyllCompile hp
        let auxFiles = fromGlob (tmp </> haddockHtmlPath h </> "*.js")
                       .||. fromGlob (tmp </> haddockHtmlPath h </> "*.png")
                       .||. fromGlob (tmp </> haddockHtmlPath h </> "*.gif")
                       .||. fromGlob (tmp </> haddockHtmlPath h </> "*.css")
        match auxFiles $ do
            route   $ relativeRoute tmp
            compile $ copyFileCompiler
        -- generate a documentation page with the haddock report and a link to the documentation
        create [fromFilePath indexhtml] $ do
            route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
            compile $ do
                let docCtx = constField "title" (haddockTitle h)
                           `mappend` constField "projectpath" (dirToRoot $ haddockPath h)
                           `mappend` constField "stdout" (Text.unpack $ resStdout res)
                           `mappend` constField "stderr" (Text.unpack $ resStderr res)
                           `mappend` constField "link" (hakyllRoute hp $ takeFileName (haddockHtmlPath h) </> "index.html")
                makeItem "" >>= loadAndApplyHTMLTemplate "templates/doc.html" docCtx >>= hakyllCompile hp
    return $ hakyllRoute hp indexhtml
        