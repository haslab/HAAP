{-# LANGUAGE OverloadedStrings #-}

module HAAP.Web.PHP.Login where

import HAAP.Utils

import Hakyll

import System.FilePath


phpRoute :: Routes
phpRoute = customRoute (\iden -> replaceExtension (toFilePath iden) "php")


phpFunRoute :: FilePath -> FilePath
phpFunRoute = \iden -> replaceExtension iden "php"


addPHPLogin :: FilePath -> Item String -> Compiler (Item String)
addPHPLogin logindb a = do
    Just dest <- getUnderlying >>= getRoute
    let phpCtx = constField "code" (itemBody a)
               `mappend` constField "logindb" (fileToRoot dest </> logindb)
    loadAndApplyTemplate "templates/login.php" phpCtx a
    