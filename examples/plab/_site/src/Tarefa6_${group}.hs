{-|
Module      : Tarefa6_${group}
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_${group} where

import LI11718

{-|
Função usada para simular um /bot/ no jogo /Micro Machines/.
Em cada instante, dado o tempo decorrido, o estado do jogo
e o identificador do jogador, toma uma ação.
-}
bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
bot tick e j = undefined