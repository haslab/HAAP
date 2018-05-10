module Main where

import LI11718

import SimulateT6

import qualified ${player1} as P1
import qualified BotNuno as P2

import System.Environment
import Text.Read

main = do
    let guiargs = GUIArgs (${mapa}) (${pista}) (${bot1}) (GUIBot P2.bot) GUINone GUINone
    correT6 guiargs