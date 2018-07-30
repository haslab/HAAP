{-
HAAP: Haskell Automated Assessment Platform

This module provides functions for processing Literate Haskell source code files.

-}

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module HAAP.Code.Literate.Haskell
    (
    lhs2hs, lhs2lhs, compileLHS
    ) where

import HAAP.Core
import HAAP.IO
import HAAP.Shelly
import HAAP.Pretty
import HAAP.Plugin

import Data.Generics
import Data.List as List
import Data.Maybe
import qualified Data.Map as Map 
import Data.Map (Map(..))

import Language.Haskell.Exts

import System.FilePath.Find as FilePath
import System.FilePath

import Control.Monad.Except

import System.IO
import Control.Monad
import Data.List
import System.FilePath
import System.Directory
import Safe
import qualified Shelly as Sh
import Shelly (Sh(..))

-- | Select only the Haskell code from a lhs file
-- the output is hs
lhs2hs :: FilePath -> FilePath -> Sh ()
lhs2hs file outfile = run True True file outfile --(addExtension (dropExtension file) "hs")

-- | Select only the latex code from a lhs file
-- the result is still lhs and not tex
lhs2lhs :: FilePath -> FilePath -> Sh ()
lhs2lhs file outfile = run True False file outfile

run doHaddock doCode file resfile = 
     do 
     when (not $ endsWith ".lhs" file) $ do
           liftIO $ putStrLn "You should invoke .lhs-files!"
     when (doHaddock) $ do 
           liftIO $ putStrLn $ "Performing Haddock-Transformation on " ++ file
           c <- shReadFile' file
           shWriteFile' resfile (perform (haddock doCode) c) 

endsWith x = elem x . tails 

perform f = unlines.f . lines

haddock:: Bool -> [String]->[String]
haddock doCode [] = []
haddock doCode a@(x:xs) 
  | startsWith "\\begin{code}" x = let (p1,p2) = splitCond (startsWith "\\end{code}") xs in 
                                        (if doCode then p1 else []) ++ haddock doCode (tailSafe p2)
  | x == [] = []:haddock doCode xs
  | doCode && headNote "haddock2" x == '>' = tailSafe x:haddock doCode xs  
  --tex code
  | doCode && startsWith "%%Haddock:" x = 
        let (p1,p2) = splitCond (not.startsWith "%%Haddock:") a in 
        "{-|":map (tailSafe . dropWhile (/=':')) p1 ++ "-}":haddock doCode p2 
  | otherwise = (if doCode then [] else [x]) ++ haddock doCode xs

startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys 

splitCond::(a->Bool)->[a]->([a],[a])
splitCond p [] = ([],[])
splitCond p (x:xs) 
  | p x = ([], x:xs)
  | otherwise = let (p1,p2) = splitCond p xs in (x:p1,p2)
  
compileLHS :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m Bool
compileLHS path = do
    let dir = takeDirectory path
    let file = takeFileName path
    let texfile = replaceExtension file "tex"
    let pdffile = replaceExtension file "pdf"
    !shok <- runBaseSh $ liftM (either (const False) (const True)) $ orEitherSh $ Sh.escaping False $ do
        --liftIO $ putStrLn $ dir
        --liftIO $ putStrLn $ file
        shCd dir
        shRm pdffile
        shCommand_ "lhs2tex" [file++">"++texfile]
        shCommand_ "pdflatex" ["-interaction=nonstopmode",texfile]
    --runBaseIO' $ ioCommand_ "pdflatex" ["-interaction=nonstopmode",dir </> texfile]
    !texok <- runBaseIO' $ doesFileExist $ dir </> pdffile
    return $! shok && texok


