{-# LANGUAGE ViewPatterns, FlexibleInstances, TypeSynonymInstances, StandaloneDeriving, DeriveGeneric #-}

module OracleT1 where

import LI11718

import Data.Typeable

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq -- package "deepseq"

import Text.PrettyPrint -- package "pretty"
import Text.PrettyPrint.GenericPretty -- package "GenericPretty"

import System.Random

import Safe

deriving instance Typeable Passo
deriving instance Typeable Mapa
deriving instance Typeable Peca
deriving instance Typeable Tipo
deriving instance Typeable Orientacao
deriving instance Typeable Carro
deriving instance Typeable Jogo
deriving instance Typeable Propriedades
deriving instance Typeable Acao

deriving instance Generic Passo
deriving instance Generic Mapa
deriving instance Generic Peca
deriving instance Generic Tipo
deriving instance Generic Orientacao
deriving instance Generic Carro
deriving instance Generic Jogo
deriving instance Generic Propriedades
deriving instance Generic Acao

instance NFData Passo
instance NFData Mapa
instance NFData Peca
instance NFData Tipo
instance NFData Orientacao
instance NFData Carro
instance NFData Jogo
instance NFData Propriedades
instance NFData Acao

instance Pretty Jogo where
    docPrec i x = doc x
    doc x = text $ show x
    
instance Pretty Propriedades where
    docPrec i x = doc x
    doc x = text $ show x

instance Pretty Acao where
    docPrec i x = doc x
    doc x = text $ show x

instance Pretty Carro where
    docPrec i x = doc x
    doc x = text $ show x

instance Pretty Orientacao where
    docPrec i x = doc x
    doc x = text $ show x

newtype PrettyMapa = PrettyMapa { unPrettyMapa :: Mapa }
  deriving (Eq,Generic)
instance NFData PrettyMapa

instance Show PrettyMapa where
    show = pretty

instance Pretty Peca where
    docPrec i x = doc x
    doc x = text $ show x
    
instance Pretty PrettyMapa where
    docPrec i x = doc x
    doc (PrettyMapa m) = doc m

instance Pretty Mapa where
    docPrec i x = doc x
    doc m@(Mapa pair tab) = text $ show m++"\n"++printTab tab

newtype PrettyTabuleiro = PrettyTabuleiro { unPrettyTabuleiro :: Tabuleiro }
  deriving (Eq,Generic)
instance NFData PrettyTabuleiro
 
instance Show PrettyTabuleiro where
    show = pretty

instance Pretty PrettyTabuleiro where
    docPrec i x = doc x
    doc (PrettyTabuleiro tab) = text $ show tab++"\n"++printTab tab

newtype PrettyCaminho = PrettyCaminho { unPrettyCaminho :: Caminho }
  deriving (Eq,Generic)
instance NFData PrettyCaminho
 
instance Show PrettyCaminho where
    show = pretty
instance Pretty PrettyCaminho where
    docPrec i x = doc x
    doc (PrettyCaminho x) = docList x

instance Pretty Passo where
    docPrec i x = doc x
    doc x = text $ show x
    docList [] = text $ []
    docList [x] = text $ show [x]
    docList (h:t) = text $ "["++show h++(concatMap ((", "++).show) t)++"]"

-------
-- Tweaks para melhorar o feedback no browser
-------
printMapa (Mapa _ tab) =
  printTab tab

printTab tab =
  concatMap ((++"\n").concatMap printPeca) tab

printHeight (Mapa _ tab) =
  putStr $
  concatMap ((++"\n").concatMap printPHeight) tab

printPHeight (Peca _ a) = if a >=0 then " "++show a else show a
printPeca (Peca t _) = printTipoPeca t
printTipoPeca :: Tipo -> String
printTipoPeca Recta  = "[]"
printTipoPeca (Rampa Norte) = "/\\"
printTipoPeca (Rampa Este)  = ">>"
printTipoPeca (Rampa Sul)   = "\\/"
printTipoPeca (Rampa Oeste) = "<<"
printTipoPeca (Curva Oeste) = "\\\\"
printTipoPeca (Curva Sul)   = "//"
printTipoPeca (Curva Norte) = "//"
printTipoPeca (Curva Este)  = "\\\\"
printTipoPeca Lava       = ".."


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

intToDouble :: Int -> Double
intToDouble = toEnum

pontoToPosicao :: Ponto -> Posicao
pontoToPosicao (x,y) = (floor x,floor y)

posicaoToPonto :: Posicao -> Ponto
posicaoToPonto (x,y) = (intToDouble x,intToDouble y)

invOrientacao Sul = Norte
invOrientacao Norte = Sul
invOrientacao Este = Oeste
invOrientacao Oeste = Este

vOri Norte = True
vOri Sul = True
vOri _ = False

hOri Este = True
hOri Oeste = True
hOri _ = False

isCurva :: Peca -> Bool
isCurva (Peca (Curva _) _) = True
isCurva _ = False

ladosPeca :: Peca -> [Orientacao]
ladosPeca (Peca t _) = ladosP t
    where
    ladosP Recta = [Norte,Sul,Este,Oeste]
    ladosP (Rampa Norte) = [Norte,Sul]
    ladosP (Rampa Sul) = [Norte,Sul]
    ladosP (Rampa Oeste) = [Oeste,Este]
    ladosP (Rampa Este) = [Oeste,Este]
    ladosP (Curva Norte) = [Sul,Este]
    ladosP (Curva Este) = [Oeste,Sul]
    ladosP (Curva Sul) = [Norte,Oeste]
    ladosP (Curva Oeste) = [Este,Norte]
    ladosP Lava = []

type Linha = (Posicao,Posicao)

paredesPeca :: Peca -> Posicao -> [Linha]
paredesPeca (Peca t _) p = paredesPeca' t p
    where
    paredesPeca' Recta p = []
    paredesPeca' (Rampa Norte) p = [paredeOeste p,paredeEste p]
    paredesPeca' (Rampa Sul) p = [paredeOeste p,paredeEste p]
    paredesPeca' (Rampa Oeste) p = [paredeNorte p,paredeSul p]
    paredesPeca' (Rampa Este) p = [paredeNorte p,paredeSul p]
    paredesPeca' (Curva Norte) p = [paredeDiag1 p]
    paredesPeca' (Curva Este) p = [paredeDiag2 p]
    paredesPeca' (Curva Sul) p = [paredeDiag1 p]
    paredesPeca' (Curva Oeste) p = [paredeDiag2 p]
    paredesPeca' Lava p = [paredeOeste p,paredeEste p,paredeNorte p,paredeSul p]
    
    paredeOeste (x,y) = ((x,y),(x,y+1))
    paredeEste (x,y) = ((x+1,y),(x+1,y+1))
    paredeNorte (x,y) = ((x,y),(x+1,y))
    paredeSul (x,y) = ((x,y+1),(x+1,y+1))
    paredeDiag1 (x,y) = ((x,y+1),(x+1,y))
    paredeDiag2 (x,y) = ((x,y),(x+1,y+1))

distanceLinhaPonto :: Linha -> Ponto -> Double
distanceLinhaPonto (posicaoToPonto -> (x1,y1),posicaoToPonto -> (x2,y2)) (x0,y0) = top / bot
    where
    top = abs $ (y2-y1)*x0 - (x2-x1)*y0 + x2*y1 - y2*x1
    bot = sqrt $ (y2-y1)^2 + (x2-x1)^2

distanceParedes :: Peca -> Posicao -> Ponto -> Double
distanceParedes peca pos pnt = sum $ map (flip distanceLinhaPonto pnt) (paredesPeca peca pos)

pecaSaida :: Peca -> Orientacao -> Orientacao
pecaSaida (Peca t _) o = pecaS t o
    where
    pecaS Recta o = o
    pecaS (Rampa _) o = o
    pecaS (Curva Este) Este = Sul
    pecaS (Curva Este) Norte = Oeste
    pecaS (Curva Oeste) Oeste = Norte
    pecaS (Curva Oeste) Sul = Este
    pecaS (Curva Norte) Norte = Este
    pecaS (Curva Norte) Oeste = Sul
    pecaS (Curva Sul) Sul = Oeste
    pecaS (Curva Sul) Este = Sul
    pecaS p o = error $ "pecaSaida " ++ show p ++ " " ++ show o  

metaOri :: Peca -> Orientacao -> Orientacao
metaOri (Peca t _) o = meta' t o
    where
    meta' Recta o = invOrientacao o
    meta' (Rampa _) o = invOrientacao o
    meta' (Curva Este) Este = Oeste
    meta' (Curva Este) Norte = Sul
    meta' (Curva Oeste) Oeste = Este
    meta' (Curva Oeste) Sul = Norte
    meta' (Curva Norte) Norte = Sul
    meta' (Curva Norte) Oeste = Este
    meta' (Curva Sul) Sul = Norte
    meta' (Curva Sul) Este = Oeste
    meta' p o = error $ "meta " ++ show p ++ " " ++ show o

metaLine :: Orientacao -> Float -> Float -> Float -> [(Float,Float)]
metaLine Norte i j tam = [(i*tam,-j*tam),((i+1)*tam,-j*tam)]
metaLine Sul i j tam = [(i*tam,-(j+1)*tam),((i+1)*tam,-(j+1)*tam)]
metaLine Este i j tam = [((i+1)*tam,-j*tam),((i+1)*tam,-(j+1)*tam)]
metaLine Oeste i j tam = [((i)*tam,-j*tam),((i)*tam,-(j+1)*tam)]


int2passo :: Int -> Passo
int2passo c = case c of
  0 -> CurvaEsq
  1 -> CurvaDir
  2 -> Sobe
  3 -> Desce
  _ -> Avanca
randomCam size seed =
    map int2passo . take size $ randomRs (0,4) (mkStdGen seed)

char2passo c = case c of
  'l' -> CurvaEsq
  'r' -> CurvaDir
  'u' -> Sobe
  'd' -> Desce
  _ -> Avanca
str2passo = map char2passo

---
testesT1 :: [Caminho]
testesT1 = testesMC_T1 ++ testesJP_T1


-- testes (Marco)
testesMC_T1 = [mc_caminho1,mc_caminho2,mc_caminho3,mc_caminho4,mc_caminho5,mc_caminho6]

mc_caminho1 = [Avanca,Avanca,Avanca,Sobe,Avanca,Avanca,Avanca,CurvaDir,Sobe,CurvaDir,Avanca,Avanca,Avanca,Avanca,Desce,Avanca,Avanca,Avanca,CurvaDir,Avanca,CurvaDir,Avanca]
mc_caminho2 = [Avanca, Avanca, Avanca, Avanca, Sobe, Avanca, Sobe, Avanca, Avanca, CurvaDir, Avanca, Desce, CurvaDir, CurvaEsq, CurvaDir, Avanca, Avanca, Avanca,CurvaEsq, Avanca, CurvaDir, Avanca, Avanca, Avanca, Desce, Avanca, CurvaDir, Avanca, Avanca, Avanca, Avanca, Avanca, CurvaDir, Avanca]
mc_caminho3 = [Avanca,Avanca,Avanca,Sobe,Avanca,Avanca,Avanca,CurvaDir,Sobe,CurvaDir,Avanca,Avanca,Avanca,Avanca,Desce,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca, Desce,CurvaDir,Avanca, CurvaDir, Avanca]
mc_caminho4 = [CurvaEsq,Avanca,Avanca,Avanca,Sobe,Avanca,Avanca,Avanca,CurvaDir,Sobe,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir, Avanca,CurvaEsq,Avanca,Avanca,Desce,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca, Desce,CurvaDir,Avanca, CurvaDir, Avanca]
mc_caminho5 = [Avanca,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Desce,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,Avanca,Desce,Avanca,Avanca,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaDir,Avanca,Avanca,Sobe,Avanca,CurvaDir,Avanca,Avanca,Avanca,Sobe,Desce,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Avanca,Sobe,Desce,Avanca,Avanca,Avanca,Avanca,CurvaEsq,Sobe,Desce,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,Avanca,Sobe,Avanca,CurvaDir,Avanca,Avanca,Avanca,Desce,Desce,Avanca,Avanca,CurvaEsq,Avanca,CurvaEsq,Avanca,Desce,Avanca,Sobe,Avanca,Avanca,Sobe,Avanca,Sobe,Avanca,Avanca]
mc_caminho6 = [Avanca,Avanca,CurvaDir,Avanca,CurvaEsq,Avanca,Sobe, Avanca,Avanca,Desce,Desce,Avanca, Avanca,CurvaEsq,CurvaDir,CurvaEsq,Avanca,Avanca,Desce,Avanca, Avanca,Avanca,CurvaEsq,CurvaEsq,Avanca,Avanca,Avanca,CurvaDir,Avanca,Desce,Avanca,CurvaDir,Avanca,Sobe,Avanca,CurvaEsq, Avanca,Avanca,Avanca,Sobe,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,Avanca,CurvaEsq,Avanca,Sobe,Avanca,CurvaEsq,Avanca]

-- testes (Jose)
testesJP_T1 = testesJP_T1_validos ++ testesJP_T1_invalidos
testesJP_T1_validos =
  [ -- simples válidos
     str2passo "flflflfl" -- quadrado para a esq
   , str2passo "rrrr" -- quadrado mínimo para a dir
   , str2passo "frffflflflfffrfr" -- caminho em 8 (cruza)
   -- simples com subidas e descidas
   , str2passo "flfrfrdflrlrrlrllffdrrffdlrurlrlfruuffrf" -- caminho principal do Nuno
   -- crazy loops
   , str2passo "ffflulldffrdrrufffflulldffrdrrufffflulldffrdrrufffflulldffrdrrufffflulldffrdrrufrfrfffrlffrlffrlffrlffrffr"
  ]
testesJP_T1_invalidos =
  [ -- simples inválidos - unitários
     str2passo "uu"
   , str2passo "dd"
   , str2passo "ll"
   , str2passo "rr"
   , str2passo "ff"
   -- caminhos random (inválidos)
   , randomCam 20 0 -- size=20, seed=0
   , randomCam 20 1
   , randomCam 20 2
   , randomCam 20 3
   , randomCam 20 4
   , randomCam 20 5
   , randomCam 20 6
  ]




