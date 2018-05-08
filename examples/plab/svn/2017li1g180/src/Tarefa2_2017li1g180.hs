module Tarefa2_2017li1g180 where

import LI11718
import Data.List
import Data.Maybe
import Safe

import Tarefa1_2017li1g180

testesT2 :: [Tabuleiro]
testesT2 = tabs

validaPos :: Posicao -> Tabuleiro -> Bool
-- hpacheco: mudei para incluir parede
validaPos (x,y) t = x <= maybe 0 length (headMay t) && y <= length t

ponto2Pos :: Ponto -> Posicao
ponto2Pos (x,y) = (i,j)
    where x' = floor x
          y' = floor y
          i = x'
          j = y'

validaPonto :: Ponto -> Tabuleiro -> Bool
-- hpacheco: mudei para incluir parede
validaPonto (a,b) t = x <= maybe 0 length (headMay t) && y <= length t && x >= 0 && y >= 0
  where (x,y) = ponto2Pos (a,b)

valida :: Mapa -> Bool
valida (Mapa _ []) = False
valida (Mapa (p,d) m) | not (validaTabuleiro m) = False
                      | c == [] = False
                      | otherwise = validaPos p m && (daVolta c) && (naoDesperdica c t) && (sequencial c a)
  where c = percorre [] m p d
        (Peca t0 x,_,_) = head c
        a = if t0 == Rampa (roda (roda d True) True) then (x+1) else x
        t = todoPiso (0,0) m

validaTabuleiro :: Tabuleiro -> Bool
validaTabuleiro m | length (nub (map length m)) > 1 = False
                  | not (bordaLava m) = False
                  | otherwise = True

bordaLava :: Tabuleiro -> Bool
bordaLava t = head t == h && last t == h && map head t == v && map last t == v
  where h = replicate (length (head t)) (Peca Lava altLava)
        v = replicate (length t) (Peca Lava altLava)

percorre :: [(Peca,Posicao,Orientacao)] -> Tabuleiro -> Posicao -> Orientacao -> [(Peca,Posicao,Orientacao)]
percorre vs m (i,j) d | i < 0 || j < 0 || i >= length (head m) || j >= length m = vs
                      | d' == Nothing = vs
                      | v `elem` vs = vs++[v]
                      | otherwise = percorre (vs++[v]) m (mexe (i,j) (fromJust d')) (fromJust d')
  where v = (atNote2 "percorre" m j i,(i,j),d)
        d' = curva d (atNote2 "percorre" m j i)


lookMap :: Tabuleiro -> Int -> Int -> Peca
lookMap xs j i | j < length xs && i < maybe 0 length (headMay xs) = atNote "lookMap" (atNote "lookMap" xs j) i 
lookMap xs j i = error $ "lookMap " ++ show xs ++ " " ++ show j ++ " " ++ show i

todoPiso :: Posicao -> [[Peca]] -> [Posicao]
todoPiso (i,j) m | j >= length m = []
                 | i >= length (head m) = todoPiso (0,j+1) m
                 | lookMap m j i == Peca Lava altLava = todoPiso (i+1,j) m
                 | otherwise = (i,j) : todoPiso (i+1,j) m

daVolta :: [(Peca,Posicao,Orientacao)] -> Bool
daVolta [] = False
daVolta [_] = False
daVolta p = (head p) == (last p)

sequencial :: [(Peca,Posicao,Orientacao)] -> Altura -> Bool
sequencial [] _ = True
sequencial ((Peca (Rampa d') a',_,d):c) a = maybe False (sequencial c) a''
  where a'' = subir (a,d) (a',d')            
sequencial ((Peca _ a',_,_):c) a | a /= a' = False
                                 | otherwise = sequencial c a

subir :: (Altura,Orientacao) -> (Altura,Orientacao) -> Maybe Altura
subir (a,d) (a',d') | d == d' && a == a' = Just (a+1)
                    | d == roda (roda d' True) True && a == a' + 1 = Just a'
                    | otherwise = Nothing

naoDesperdica :: [(Peca,Posicao,Orientacao)] -> [Posicao] -> Bool
naoDesperdica c p = length (nub (map (\(_,x,_) -> x) c)) == length (nub p)

curva :: Orientacao -> Peca -> Maybe Orientacao
curva o (Peca (Curva c) _) | o == c = Just $ roda o True
                           | o == (roda c False) = Just $ roda o False
                           | otherwise = Nothing
curva _ (Peca Lava _) = Nothing
curva o _ = Just o


tabs = [mm_ex1,mm_ex2,mm_ex3,mm_exPI,mm_exLV,mm_exEX,mm_exLH,mm_why,mm_NSq,mm_Rec,mm_Rec']

-- mapa nao geravel por caminhos, lava extra a volta
mm_ex1 = [[Peca Lava 2, Peca Lava 2, Peca Lava 2, Peca Lava 2]
        ,[Peca Lava 2, Peca (Curva Norte) 2,Peca (Curva Este) 2, Peca Lava 2]
        ,[Peca Lava 2, Peca (Curva Oeste) 2,Peca (Curva Sul) 2, Peca Lava 2]
        ,[Peca Lava 2, Peca Lava 2, Peca Lava 2, Peca Lava 2]]

-- mapa nao geravel por caminhos, altura /= inicial sem possibilidade de rampas
mm_ex2 = [[Peca (Curva Norte) 5,Peca (Curva Este) 5],[Peca (Curva Oeste) 5,Peca (Curva Sul) 5]]

-- mapa minimo sem vizinhos
mm_ex3 = [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Lava 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]


-- posicao inicial invalida
mm_exPI = [[Peca (Curva Norte) 2,Peca (Curva Este) 2],[Peca (Curva Oeste) 2,Peca (Curva Sul) 2]]

-- mapa so lava
mm_exLV = theFloorIsLava (5,10)

-- mapa com caminho extra
mm_exEX = [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Recta 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]

-- altura da lava invalida
mm_exLH = [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Lava 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]

mm_why = [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Recta 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]

mm_NSq = [[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
        ,[Peca Lava altLava, Peca (Curva Norte) 5,Peca (Curva Este) 5, Peca Lava altLava, Peca Lava altLava]
        ,[Peca Lava altLava, Peca (Curva Oeste) 5,Peca (Curva Sul) 5, Peca Lava altLava]
        ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
        ]

mm_Rec = [[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
        ,[Peca Lava altLava, Peca Recta 5,Peca Lava altLava]
        ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
        ]                          

mm_Rec' = [[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
         ,[Peca Recta 5, Peca Recta 5,Peca Recta 5]
         ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
         ]