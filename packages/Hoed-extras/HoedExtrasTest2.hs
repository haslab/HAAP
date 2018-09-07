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

    mylength :: [a] -> Int
    mylength [] = 0
    mylength (x:xs) = succ (mylength xs)
    
    |]

main1 = do
    let args = defaultHoedExtrasArgs { jshood = View }
    runHoedExtrasO args (print $ mylength  "abcd")

main2 = do
    let args = defaultHoedExtrasArgs { ghood = View }
    runHoedExtrasO args (print $ mylength  "abcd")

main3 = do
    let args = defaultHoedExtrasArgs { jshoed = View }
    runHoedExtrasO args (print $ mylength  "abcd")
    
main4 = do
    let args = defaultHoedExtrasArgs { ghoed = View }
    runHoedExtrasO args (print $ mylength  "abcd")

main5 = do
    let args = defaultHoedExtrasArgs { jshoedb = View }
    runHoedExtrasO args (print $ mylength  "abcd")

