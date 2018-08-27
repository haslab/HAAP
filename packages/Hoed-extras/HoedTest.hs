{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports, RecordWildCards #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wno-partial-type-signatures #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns, TupleSections #-}

import qualified "debug" Debug.Hoed as Debug
import "Hoed" Debug.Hoed
import Debug.Hoed.JsHood as JsHood
import Debug.Hoed.JsHoed
import Debug.Hoed.Extras
import Debug.Hoed.GHood
import Debug.Hoed.GHoed
import Debug.Hoed.CompTree.Exts
--import Debug.Observe
import qualified Data.Text as T
import Data.Aeson
import Control.Monad
import qualified Data.ByteString as B

import Data.Serialize as S

import qualified GHC.Generics

Debug.debug' Debug.Config{Debug.generateGenericInstances=True,Debug.generateObservableInstances=True, Debug.excludeFromInstanceGeneration=[]} [d|

    isOddEven :: Int -> (Bool,Bool)
    isOddEven i = (isOdd i,isEven i)

    isOdd :: Int -> Bool
    isOdd 0 = False
    isOdd 1 = True
    isOdd n = isEven (minus n one)

    isEven :: Int -> Bool
    isEven 0 = True
    isEven 1 = False
    isEven n = isOdd (minus n one)

    minus :: Int -> Int -> Int
    minus n m = n - m

    one :: Int
    one = 1
    
    |]

--isOddEven :: Int -> (Bool,Bool)
--isOddEven = observe "isOddEven" isOddEven'
--isOddEven' i = (isOdd i,isEven i)
--
--isOdd :: Int -> Bool
--isOdd = observe "isOdd" isOdd'
--isOdd' 0 = False
--isOdd' 1 = True
--isOdd' n = isEven (minus n one)
--
--isEven :: Int -> Bool
--isEven = observe "isEven" isEven'
--isEven' 0 = True
--isEven' 1 = False
--isEven' n = isOdd (minus n one)
--
--minus :: Int -> Int -> Int
--minus = observe "minusOne" minus'
--minus' n m = n - m
--
--one :: Int
--one = observe "one" one'
--one' = 1

-- Java Hood Java interface
main1 = do
    runGHoodO (print $ isOddEven 5)

-- Client-server Web Hoed interface
main2 = do
    runGHoedO (print $ isOddEven 5)
    
-- Client Web Hoed interface
main3 = do
    runJsHoedO (print $ isOddEven 5)

-- Client Web debug interaace
main4 = Debug.debugRun (print $ isOddEven 5)

-- Client Web Hood interface
main5 = do
    runJsHoodO (print $ isOddEven 5)
    
-- analyse the program
main6 = do
    h <- runO' defaultHoedOptions (print $ isOddEven 5)
    return ()
