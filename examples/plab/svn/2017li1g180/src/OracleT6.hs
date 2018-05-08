{-# LANGUAGE PatternGuards #-}

module OracleT6 where

import LI11718
import OracleT1
import OracleT3
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Safe
--import Debug.Trace
import Data.Fixed
import Mapas

testesT6 :: [Mapa]
testesT6 = (constroi ((replicate 20 Avanca)++[CurvaDir,Avanca,Avanca,CurvaDir]++(replicate 20 Avanca)++[CurvaDir,Avanca,Avanca,CurvaDir])): mapas128 ++ mapas64 ++ mapas16 -- ++ mapas4