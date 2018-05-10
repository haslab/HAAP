module Main where

import LI11718

import SimulateT6

import qualified ${player1} as P1
import qualified ${player2} as P2
import qualified ${player3} as P3
import qualified ${player4} as P4

import System.Environment
import Text.Read

main = do
    let guiargs = GUIArgs (${mapa}) (${pista}) (${bot1}) (${bot2}) (${bot3}) (${bot4})
    simulaT6 guiargs