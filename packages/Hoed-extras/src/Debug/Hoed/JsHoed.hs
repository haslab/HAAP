{-# LANGUAGE OverloadedStrings, PackageImports, RecordWildCards #-}

module Debug.Hoed.JsHoed where

import "Hoed" Debug.Hoed
import Debug.Hoed.Utils
import Debug.Hoed.Observe
import Debug.Hoed.CompTree
import Debug.Hoed.CompTree.Exts

import System.FilePath.Posix
import System.FilePath.Glob
import System.IO.Temp
import System.IO
import System.Process
import Web.Browser

import Paths_Hoed_extras

import Control.Monad

import qualified Data.Text as T

jshoedExtra :: HoedExtrasArgs -> HoedAnalysis -> IO ()
jshoedExtra args h = case jshoed args of
    None -> return ()
    View -> debugSession h
    Deploy -> case datapath args of
        Nothing -> do
            debugOutput "." h >>= writeFile "jshoed.html"
            files <- jsHoedFiles
            path <- getDataFileName "."
            forM_ files $ \file -> system $ "cp " ++ file ++ " " ++ (makeRelative path file)
        Just path -> path >>= flip debugOutput h >>= writeFile "jshoed.html"

runJsHoedO :: IO a -> IO ()
runJsHoedO = runJsHoedOwith defaultHoedOptions

runJsHoedOwith :: HoedOptions -> IO a -> IO ()
runJsHoedOwith options program = do
  h <- runO' options program
  debugSession h
  return ()
 
debugOutput :: FilePath -> HoedAnalysis -> IO String
debugOutput datapath h = do
    let ct = hoedCompTree h
    let ctx "input" = T.pack $ showCompTree ct
        ctx "datapath" = T.pack $ datapath
        ctx n = n
    infn <- getDataFileName "web/jshoed.html"
    tplt <- readTemplateFile infn
    let outstr = applyTemplate tplt ctx
    return outstr
 
debugSession :: HoedAnalysis -> IO ()
debugSession h = do
    datapath <- getDataFileName "."
    outstr <- debugOutput datapath h
    outfn <- writeSystemTempFile "jshoed.html" outstr
    openBrowser outfn
    return ()

jsHoedFiles :: IO [FilePath]
jsHoedFiles = do
    libdir <- getDataFileName "."
    js <- glob (libdir </> "web/JSHoed.jsexe/*.js")
    png <- glob (libdir </> "img/*.png")
    return $ js ++ png
    

    
