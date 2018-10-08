{-
HAAP: Haskell Automated Assessment Platform

This module runs the @GHC@ plugin with the @-Wall@ flag to give all kinds of warnings on code quality.

-}

{-# LANGUAGE EmptyDataDecls, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, OverloadedStrings #-}

module HAAP.Code.GHC
    ( GHCWallArgs(..)
    , runGHCWall
    ) where

import HAAP.Compiler.GHC
import HAAP.Core
import HAAP.IO
import HAAP.Web.Hakyll
import HAAP.Utils
import HAAP.Plugin
import HAAP.Shelly

import Data.Default
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as Text
import Data.Proxy

import Control.Monad.Reader as Reader

import System.FilePath


data GHCWallArgs = GHCWallArgs
    { ghcWallSandbox :: Sandbox
    , ghcWallArgs :: [String]
    , ghcWallPath :: FilePath -- path relative to the project where to execute the ghc plugin
    , ghcWallFiles :: [FilePath] -- relative to the path where homplexity is executed
    , ghcWallHtmlPath :: FilePath -- relative to the project path
    }

runGHCWall :: (MonadIO m,HasPlugin Hakyll t m) => GHCWallArgs -> Haap t m FilePath
runGHCWall wall = do
    hp <- getHakyllP
    let homerrorpath = ghcWallHtmlPath wall
    orErrorHakyllPage homerrorpath homerrorpath $ do
        tmp <- getProjectTmpPath
        let ioArgs = def { ioSandbox = mapSandboxCfg (dirToRoot (ghcWallPath wall) </>) (ghcWallSandbox wall) }
        let extras = ghcWallArgs wall
        let files = ghcWallFiles wall
        let ghcargs = def { ghcArgs = "-Wall":extras, ghcIO = ioArgs }
        res <- orErrorWritePage (tmp </> ghcWallHtmlPath wall) mempty $ runBaseSh $ do
            shCd $ ghcWallPath wall
            shGhcWith ghcargs files
        let messages = parseMessages $ splitWhen isEmptyLine $ lines (Text.unpack $ resStdout res `Text.append` resStderr res)
        hakyllFocus ["templates"] $ hakyllRules $ do
            -- copy the generated documentation
            create [fromFilePath $ ghcWallHtmlPath wall] $ do
                route $  idRoute `composeRoutes` funRoute (hakyllRoute hp)
                compile $ do
                    let msgCtx = field "class" (return . fst3 . itemBody)
                               `mappend` field "suggestion" (return . snd3 . itemBody)
                               `mappend` field "message" (return . thr3 . itemBody)
                    let homCtx = constField "projectpath" (fileToRoot $ hakyllRoute hp $ ghcWallHtmlPath wall)
                               `mappend` listField "messages" msgCtx (mapM makeItem messages)
                    makeItem "" >>= loadAndApplyHTMLTemplate "templates/ghcwall.html" homCtx >>= hakyllCompile hp
        return (hakyllRoute hp $ ghcWallHtmlPath wall)
      
parseMessages = catMaybes . map parseMessage
parseMessage [] = Nothing
parseMessage (x:xs)
    | isInfixOf "warning:" x = Just ("hspec-warning",x,unlines xs)
    | isInfixOf "error:" x = Just ("hspec-failure",x,unlines xs)
    | otherwise = Nothing --Just ("hspec-debug",x,unlines xs) 


  