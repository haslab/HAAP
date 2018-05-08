module Main where

import LI11718
import qualified Tarefa3_2017li1g180 as T3

import System.Environment
import Text.Read

main = do
    args <- getArgs
    case args of
        ["movimenta"] -> do
            str <- getContents
            let params = readMaybe str
            case params of
                Nothing -> error "parâmetros inválidos"
                Just (mapa,tempo,carro) -> print $ T3.movimenta mapa tempo carro
        ["testes"] -> print $ T3.testesT3
        otherwise -> error "RunT3 argumentos inválidos"