module Main where

import LI11718
import qualified Tarefa2_${group} as T2

import System.Environment
import Text.Read

main = do
    args <- getArgs
    case args of
        ["valida"] -> do
            str <- getContents
            let mapa = readMaybe str
            case mapa of
                Nothing -> error "mapa inválido"
                Just m -> print $$ T2.valida m
        ["testes"] -> print $$ T2.testesT2
        otherwise -> error "RunT2 argumentos inválidos"