{-# LANGUAGE OverloadedStrings #-}

module HAAP.Web.PHP.Login where

import HAAP.Utils

import Hakyll

import System.FilePath


phpRoute :: Routes
phpRoute = customRoute $ \iden -> case takeExtension (toFilePath iden) of
    ".html" -> replaceExtension (toFilePath iden) "php"
    otherwise -> toFilePath iden


phpFunRoute :: FilePath -> FilePath
phpFunRoute = \iden -> case takeExtension iden of
    ".html" -> replaceExtension iden "php"
    otherwise -> iden


addPHPLogin :: FilePath -> Item String -> Compiler (Item String)
addPHPLogin logindb a = do
    Just dest <- getUnderlying >>= getRoute
    let phpCtx = constField "code" (itemBody a)
               `mappend` constField "logindb" (fileToRoot dest </> logindb)
    loadAndApplyTemplate "templates/login.php" phpCtx a
    