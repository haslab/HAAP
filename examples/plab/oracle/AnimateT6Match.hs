module Main where

import LI11718

import SimulateT6
import AnimateT6

import System.Environment
import Text.Read

main = do
    animaT6 (${mapa}) (${pista}) (${frames}) (${players})