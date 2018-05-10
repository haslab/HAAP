module OracleT2 where

import LI11718
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Safe

import OracleT1

getPeca :: Posicao -> Tabuleiro -> Peca
getPeca (i,j) tab = tab !! j !! i

getPecaAtual :: Jogo -> Int -> Peca
getPecaAtual jogo p = peca
  where
    Mapa _ tab = mapa jogo
    peca = getPeca (pontoToPosicao $ posicao $ getCarro p jogo) tab
    
getCarro p jogo = atNote "no carro" (carros jogo) p

normalizaPosicao :: Tabuleiro -> Posicao -> Posicao
normalizaPosicao tab (x,y) = (max 0 $ min x lx,max 0 $ min y ly)
    where
    lx = length (head tab)
    ly = length tab

orientacoes :: [Orientacao]
orientacoes = [Norte,Sul,Este,Oeste]

genOrientacao :: Gen Orientacao
genOrientacao = elements orientacoes

genPonto :: Tabuleiro -> Gen Ponto
genPonto t = do
    x <- choose (0,maxx-1)
    y <- choose (0,maxy-1)
    return (x,y)
  where
    maxx = maybe 0 (realToFrac . length) (headMay t)
    maxy = realToFrac $ length t

genPosicao :: Tabuleiro -> Gen Posicao
genPosicao t = do
    x <- choose (0,maxx-1)
    y <- choose (0,maxy-1)
    return (x,y)
  where
    maxx = maybe 0 length (headMay t)
    maxy = length t

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

tamanhoMapa :: Mapa -> Int
tamanhoMapa = length . pecasMapa

pecasMapa :: Mapa -> [(Peca,Posicao,Orientacao)]
pecasMapa (Mapa (p,o) tab) = percorre [] tab p o

percorre :: [(Peca,Posicao,Orientacao)] -> Tabuleiro -> Posicao -> Orientacao -> [(Peca,Posicao,Orientacao)]
percorre vs m (i,j) d | i < 0 || j < 0 || i >= length (head m) || j >= length m = vs
                      | d' == Nothing = vs
                      | v `elem` vs = vs++[v]
                      | otherwise = percorre (vs++[v]) m (mexe (i,j) (fromJustNote "percorre" d')) (fromJustNote "percorre" d')
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

centroPeca' :: Posicao -> Ponto
centroPeca' (x,y) = (intToDouble x+0.5,intToDouble y+0.5)

centroPeca :: Tipo -> Posicao -> Ponto
centroPeca (Curva Norte) (a,b) = (toEnum a+0.7,toEnum b+0.7)
centroPeca (Curva Este) (a,b) = (toEnum a+0.3,toEnum b+0.7)
centroPeca (Curva Sul) (a,b) = (toEnum a+0.3,toEnum b+0.3)
centroPeca (Curva Oeste) (a,b) = (toEnum a+0.7,toEnum b+0.3)
centroPeca _ (a,b) = (toEnum a+0.5,toEnum b+0.5)

testesT2 :: [Tabuleiro]
testesT2 = testesMC_T2 ++ testesNM_T2 ++ testesJP_T2


-- testes (Marco)
testesMC_T2 = [mc_map_v1, mc_map_v2, mc_map_v3, mc_map_i1, mc_map_i2, mc_map_i3, mc_map_i4]

-- validos
mc_map_v1 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Rampa Este) (-1),Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-2),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) (-2),Peca Recta (-2),Peca (Rampa Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca Recta (-2),Peca (Rampa Este) (-2),Peca Recta (-1),Peca (Rampa Este) (-1),Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
mc_map_v2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca (Rampa Oeste) (-2),Peca Recta (-2),Peca Recta (-2),Peca Recta (-2),Peca (Curva Este) (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-2),Peca (Curva Este) (-2),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Recta (-2),Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Recta (-2),Peca Lava 0],[Peca Lava 0,Peca Recta (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-3),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-2),Peca Recta (-2),Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) (-1),Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) (-3),Peca Recta (-3),Peca (Rampa Este) (-3),Peca Recta (-2),Peca (Curva Sul) (-2),Peca (Rampa Sul) (-2),Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) (-1),Peca (Curva Sul) (-1),Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca (Rampa Oeste) 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca Recta (-1),Peca (Curva Sul) (-1),Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
mc_map_v3 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca (Rampa Este) 1,Peca Recta 2,Peca Recta 2,Peca (Curva Este) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- invalidos
-- altura final /= altura inicial
mc_map_i1 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
-- sem borda lateral
mc_map_i2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca Recta 2,Peca (Curva Este) 2,Peca Lava 0],[Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],[Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0],[Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0],[Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca (Curva Oeste) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
-- direcao final /= direcao inicial
mc_map_i3 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Recta 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Curva Este) 1,Peca Lava 0],[Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta 1,Peca Recta 1,Peca (Rampa Este) 1,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
-- pecas soltas
mc_map_i4 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Norte) 1,Peca (Rampa Este) 1,Peca Recta 2,Peca Recta 2,Peca (Curva Este) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Sul) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 2,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 1,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0,Peca (Rampa Norte) 2,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]


-- testes (Nuno - adicionados por Jose)
testesNM_T2 = [
    -- mapa nao geravel por caminhos, lava extra a volta
    [[Peca Lava 2, Peca Lava 2, Peca Lava 2, Peca Lava 2]
     ,[Peca Lava 2, Peca (Curva Norte) 2,Peca (Curva Este) 2, Peca Lava 2]
     ,[Peca Lava 2, Peca (Curva Oeste) 2,Peca (Curva Sul) 2, Peca Lava 2]
     ,[Peca Lava 2, Peca Lava 2, Peca Lava 2, Peca Lava 2]]
    -- mapa nao geravel por caminhos, altura /= inicial sem possibilidade de rampas
  , [[Peca (Curva Norte) 5,Peca (Curva Este) 5],[Peca (Curva Oeste) 5,Peca (Curva Sul) 5]]
    -- mapa minimo sem vizinhos
  , [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Lava 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]
    -- posicao inicial invalida
  , [[Peca (Curva Norte) 2,Peca (Curva Este) 2],[Peca (Curva Oeste) 2,Peca (Curva Sul) 2]]
    -- mapa so lava
  , theFloorIsLava (5,10)
    -- mapa com caminho extra
  , [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Recta 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]
    -- altura da lava invalida
  , [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Lava 0,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]
  ]

-- Apenas os testes usados na T1
testesJP_T2 = map (unTab.constroi) testesJP_T1
unTab (Mapa _ t) = t


