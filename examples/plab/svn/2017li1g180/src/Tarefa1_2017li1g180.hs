module Tarefa1_2017li1g180 where

import LI11718

import Data.Typeable

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq -- package "deepseq"

import Text.PrettyPrint -- package "pretty"
import Text.PrettyPrint.GenericPretty -- package "GenericPretty"

import Safe

constroi :: Caminho -> Mapa
constroi c = Mapa (partida c,dirInit) $ processa c dirInit altInit (partida c) (theFloorIsLava (dimensao c))

--------------
-- == T1: Solução
--------------

--mexe :: (Int,Int) -> Orientacao -> (Int,Int)
--mexe (x,y) Este  = (x+1,y)
--mexe (x,y) Sul   = (x,y+1)
--mexe (x,y) Oeste = (x-1,y)
--mexe (x,y) Norte = (x,y-1)
--
--roda :: Orientacao -> Bool -> Orientacao
--roda Este True  = Sul
--roda Sul True   = Oeste
--roda Oeste True = Norte
--roda Norte True = Este
--roda d False = roda (roda (roda d True) True) True

theFloorIsLava :: Dimensao -> [[Peca]]
theFloorIsLava (n,m) = replicate m (replicate n (Peca Lava altLava)) 

processa :: Caminho -> Orientacao -> Altura -> (Int,Int) -> [[Peca]] -> [[Peca]]
processa [] _ _ _ m = m
processa (CurvaDir:c) d a (x,y) m = processa c d' a (mexe (x,y) d') m'
    where m' = replace m (x,y) (blocoCurvo d d' a)
          d' = roda d True
processa (CurvaEsq:c) d a (x,y) m = processa c d' a (mexe (x,y) d') m'
    where m' = replace m (x,y) (blocoCurvo d d' a)
          d' = roda d False
processa (Avanca:c) d a (x,y) m = processa c d a (mexe (x,y) d) m'
    where m' = replace m (x,y) (Peca Recta a)
processa (s:c) d a (x,y) m = processa c d a' (mexe (x,y) d) m'
    where m' = replace m (x,y) p'
          a' = adapta s a
          p' = (blocoRampa s d) (min a a')

replace :: [[a]] -> (Int,Int) -> a -> [[a]]
replace m (x,y) e = (take y m) ++ [l] ++ (drop (y+1) m)
    where l = (take x (atNote "replace1" m y)) ++ [e] ++ (drop (x+1) (atNote "replace2" m y))

blocoCurvo :: Orientacao -> Orientacao -> Altura -> Peca
blocoCurvo Norte Este  = Peca (Curva Norte) 
blocoCurvo Este Sul    = Peca (Curva Este)  
blocoCurvo Sul Oeste   = Peca (Curva Sul)   
blocoCurvo Oeste Norte = Peca (Curva Oeste) 
blocoCurvo m n = blocoCurvo (roda (roda n True) True) (roda (roda m True) True)
-- Este Norte == Sul Oeste
-- Sul Este == Oeste Norte

adapta :: Passo -> Altura -> Altura
adapta Sobe  a = a+1
adapta Desce a = a-1
adapta _     a = a

blocoRampa :: Passo -> Orientacao -> (Altura -> Peca)
blocoRampa Sobe Norte = Peca (Rampa Norte)
blocoRampa Sobe Oeste = Peca (Rampa Oeste)
blocoRampa Sobe Sul   = Peca (Rampa Sul)
blocoRampa Sobe Este  = Peca (Rampa Este)
blocoRampa Desce d    = blocoRampa Sobe (roda (roda d True) True)

mexe :: (Int,Int) -> Orientacao -> (Int,Int)
mexe (x,y) Este  = (x+1,y)
mexe (x,y) Sul   = (x,y+1)
mexe (x,y) Oeste = (x-1,y)
mexe (x,y) Norte = (x,y-1)


roda :: Orientacao -> Bool -> Orientacao
roda Este True  = Sul
roda Sul True   = Oeste
roda Oeste True = Norte
roda Norte True = Este
roda d False = roda (roda (roda d True) True) True


atNote2 str xys x y = atNote str (atNote str xys x) y


--------------- from Nuno
testesT1 :: [Caminho]
testesT1 = [c_ex1,c_ex1',c_ex2,c_ex4,c_ex5,c_ex6
           ,c_exOP,c_exDM,c_exOL,c_exHM,c_exR,c_exE]

           -- ,m_ex1,m_ex2,m_ex3
           -- ,m_exPI,m_exLV,m_exEX,m_exLH]

c_ex1 :: Caminho
c_ex1 = [Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Desce,Avanca,CurvaEsq,CurvaDir
      ,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaEsq,Avanca,Avanca
      ,Desce,CurvaDir,CurvaDir,Avanca,Avanca,Desce,CurvaEsq,CurvaDir,Sobe,CurvaDir
      ,CurvaEsq,CurvaDir,CurvaEsq,Avanca,CurvaDir,Sobe,Sobe,Avanca,Avanca,CurvaDir,Avanca]

c_ex1' :: Caminho
c_ex1' = [Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Sobe,Avanca,CurvaEsq,CurvaDir
      ,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaEsq,Avanca,Avanca
      ,Sobe,CurvaDir,CurvaDir,Avanca,Avanca,Sobe,CurvaEsq,CurvaDir,Desce,CurvaDir
      ,CurvaEsq,CurvaDir,CurvaEsq,Avanca,CurvaDir,Desce,Desce,Avanca,Avanca,CurvaDir,Avanca]

c_ex2 :: Caminho
c_ex2 = [Avanca,CurvaEsq,CurvaEsq,Avanca,CurvaEsq,CurvaEsq]

-- mapa sobreposto, altura /= da inicial
c_ex3 :: Caminho
c_ex3 = [Desce,CurvaEsq,CurvaEsq,Desce,CurvaEsq,CurvaEsq
        ,Avanca,CurvaEsq,CurvaEsq,Avanca,CurvaEsq,CurvaEsq]

-- caminho em 8, cruza
c_ex4 :: Caminho
c_ex4 = [Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca
        ,CurvaEsq,Avanca,Avanca,Avanca,CurvaDir,Avanca,CurvaDir]

-- caminho minimo válido
c_ex5 :: Caminho
c_ex5 = [CurvaDir,CurvaDir,CurvaDir,CurvaDir]
      
-- caminho minimo sem vizinhos
c_ex6 :: Caminho
c_ex6 = [Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaDir,Avanca,CurvaDir]

-- mapa nao geravel por caminhos, lava extra a volta
m_ex1 = Mapa ((2,1),Este) [[Peca Lava 2, Peca Lava 2, Peca Lava 2, Peca Lava 2]
                          ,[Peca Lava 2, Peca (Curva Norte) 2,Peca (Curva Este) 2, Peca Lava 2]
                          ,[Peca Lava 2, Peca (Curva Oeste) 2,Peca (Curva Sul) 2, Peca Lava 2]
                          ,[Peca Lava 2, Peca Lava 2, Peca Lava 2, Peca Lava 2]]

-- mapa nao geravel por caminhos, altura /= inicial sem possibilidade de rampas
m_ex2 = Mapa ((1,0),Este) [[Peca (Curva Norte) 5,Peca (Curva Este) 5],[Peca (Curva Oeste) 5,Peca (Curva Sul) 5]]

-- mapa minimo sem vizinhos
m_ex3 = Mapa ((1,0),Este) [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Lava 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]


-- testes invalidos
-- aberto
c_exOP :: Caminho
c_exOP = [Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,CurvaDir,CurvaDir
         ,Avanca,Avanca,Avanca,CurvaDir,CurvaEsq,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca]      

-- fecha mas direcao errada
c_exDM :: Caminho
c_exDM = [Sobe,CurvaEsq,CurvaEsq,Sobe,CurvaEsq,CurvaEsq
         ,Avanca,CurvaEsq,CurvaEsq,Avanca,CurvaEsq,Avanca]

-- overlaps, aberto
c_exOL :: Caminho
c_exOL = [Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,CurvaDir,CurvaDir
         ,Avanca,CurvaDir,Avanca,CurvaDir,CurvaEsq,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca]              

-- height mismatch
c_exHM :: Caminho
c_exHM = [Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,CurvaDir,CurvaDir
         ,Avanca,Sobe,Avanca,CurvaDir,CurvaEsq,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca]

-- cruza com alturas invalidas
c_exR :: Caminho
c_exR = [Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaEsq,Sobe,CurvaEsq,Avanca
        ,CurvaEsq,Avanca,Avanca,Avanca,CurvaDir,Desce,CurvaDir]

{-
let map = constroi c_exR
printHeight map
-}

-- caminho vazio
c_exE :: Caminho
c_exE = []
