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
import Debug.Hoed.Extras

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

main1 = do
    let args = defaultHoedExtrasArgs { jshood = View }
    runHoedExtrasO args (print $ isOddEven 5)

main2 = do
    let args = defaultHoedExtrasArgs { ghood = View }
    runHoedExtrasO args (print $ isOddEven 5)

main3 = do
    let args = defaultHoedExtrasArgs { jshoed = View }
    runHoedExtrasO args (print $ isOddEven 5)
    
main4 = do
    let args = defaultHoedExtrasArgs { ghoed = View }
    runHoedExtrasO args (print $ isOddEven 5)

