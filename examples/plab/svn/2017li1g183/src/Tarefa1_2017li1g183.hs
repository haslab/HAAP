module Tarefa1_2017li1g183 where

import LI11718

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

constroi :: Caminho -> Mapa
constroi c = Mapa (partida c,dirInit) tab
  where Mapa (finishPos,finishDir) tab =
          constroiAux (Mapa (partida c,dirInit) (initTab c)) altInit c

initTab :: Caminho -> Tabuleiro
initTab c = replicate y $ replicate x  $ Peca Lava altLava
  where (x,y) = dimensao c 

constroiAux :: Mapa -> Altura -> Caminho -> Mapa
constroiAux m _ [] = m
constroiAux (Mapa (pos,dir) tab) a (i:is) =
  constroiAux (Mapa (p',d') novoMapa) a' is
  where
    (p',d',a',novoMapa) = insere i pos dir a tab

insere Avanca   pos dir a tab = (go pos dir, dir, a,   coloca (Peca Recta               a) pos tab)
insere Sobe     pos dir a tab = (go pos dir, dir, a+1, coloca (Peca (Rampa dir)         a) pos tab)
insere Desce    pos dir a tab = (go pos dir, dir, a-1, coloca (Peca (Rampa (swap dir)) (a-1)) pos tab)
-- insere CurvaEsq pos dir a tab = (go pos des, des, a,   coloca (Peca (Curva (tr dir))    a) pos tab)
insere CurvaEsq pos dir a tab = (go pos des, des, a,   coloca (Peca (Curva (dir))    a) pos tab)
  where des = tr.tr.tr$dir
insere CurvaDir pos dir a tab = (go pos ddi, ddi, a,   coloca (Peca (Curva dir)         a) pos tab)
  where ddi = tr dir

swap = tr.tr

coloca :: a -> Posicao -> [[a]] -> [[a]]
coloca peca (0,0) ((_:l):ls) = (peca:l):ls
coloca p (x,0) ((h:t):ls) = let (a2:b2) = coloca p (x-1,0) (t:ls)
                            in (h:a2):b2
coloca p (x,y) (l:ls) = l : (coloca p (x,y-1) ls)

go (x,y) Norte = (x,y-1)
go (x,y) Sul   = (x,y+1)
go (x,y) Este  = (x+1,y)
go (x,y) Oeste = (x-1,y)

tr Norte = Este
tr Este  = Sul
tr Sul   = Oeste
tr Oeste = Norte