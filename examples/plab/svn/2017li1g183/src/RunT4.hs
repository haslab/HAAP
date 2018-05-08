module Main where

import LI11718
import qualified Tarefa4_2017li1g183 as T4

import System.Environment
import Text.Read

main = do
    args <- getArgs
    case args of
        ["atualiza"] -> do
            str <- getContents
            let params = readMaybe str
            case params of
                Nothing -> error "parâmetros inválidos"
                Just (tempo,jogo,jogador,acao) -> print $ T4.atualiza tempo jogo jogador acao
        ["testes"] -> print $ take 100 $ T4.testesT4
        otherwise -> error "RunT4 argumentos inválidos"