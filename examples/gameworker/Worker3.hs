{-# LANGUAGE GADTs, CPP, Trustworthy, TemplateHaskell, TupleSections, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

module Main where

import JavaScript.Web.Worker.Extras 

import qualified Tarefa6_li1g147 as G147

main :: IO ()
main = runSyncWorker $ \(inp::[String],player::Int,ticks::Int) -> G147.bot inp player ticks







