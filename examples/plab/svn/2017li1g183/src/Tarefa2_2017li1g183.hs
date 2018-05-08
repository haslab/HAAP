module Tarefa2_2017li1g183 where

import LI11718


testesT2 :: [Tabuleiro]
testesT2 = [tt_nok2, tt_nok5, tt_nok6, tt_nok8]

valida :: Mapa -> Bool
valida (Mapa _ (_:(_:(Peca (Curva Norte) 0):_):_)) = True
valida _ = False


         
tt_nok2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

tt_nok3 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Curva Este) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Este) 0,Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Curva Norte) (-1),Peca (Rampa Este) (-1),Peca (Curva Sul) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

tt_nok4 = [[Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Curva Este) 0]
        ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Curva Norte) (-1),Peca (Rampa Este) (-1),Peca (Curva Sul) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

tt_nok5 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Sul) 0,Peca (Curva Este) 0,Peca (Curva Norte) 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Recta 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca (Curva Sul) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

         
tt_nok6 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0]]

tt_nok7 = [[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Curva Este) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca (Curva Este) 0,Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Curva Norte) (-1),Peca (Rampa Este) (-1),Peca (Curva Sul) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

tt_nok8 = [[Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Curva Este) 0]
        ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca (Rampa Este) (-1),Peca (Curva Norte) (-1),Peca (Rampa Este) (-1),Peca (Curva Sul) 0,Peca Lava 0]
        ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

                  
