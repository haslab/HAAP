{-# LANGUAGE OverloadedStrings #-}

module HAAP.Web.PHP.Login where

import Hakyll

import System.FilePath


phpRoute :: Routes
phpRoute = customRoute (\iden -> replaceExtension (toFilePath iden) "php")

addPHPLogin :: FilePath -> Item String -> Compiler (Item String)
addPHPLogin logindb a = do
    let phpCtx = constField "code" (itemBody a)
               `mappend` constField "logindb" logindb
    loadAndApplyTemplate "templates/login.php" phpCtx a
    