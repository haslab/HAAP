{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import JSImages
import LI11718
import OracleT4
import OracleT3
import OracleT2
import OracleT1
import System.Environment
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Safe
import Data.Char
import Debug.Trace
import GHC.Float
import Text.Printf
import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Monad
import Control.Exception

import qualified Data.Text as Text
import Text.Read

import qualified CodeWorld as CW
import Graphics.Gloss hiding ((.*.),(.+.),(.-.))
--import Graphics.Gloss
--import Graphics.Gloss.Data.Picture          
--import Graphics.Gloss.Interface.Pure.Game   
--import Graphics.Gloss.Juicy
--import Graphics.Gloss.Geometry.Line

standard = Propriedades (1.5) 1 4 2 15 90

parado = Acao False False False False Nothing

data Estado = Estado 
  { jogo :: Jogo 
--  , acoes :: [Acao]
  , tamBloco :: Float
  , batotas :: [Float]
  , mortes :: [Float]
  , imagens :: [(String,Picture)]
  }

--centroPeca :: Tipo -> Posicao -> Ponto
--centroPeca (Curva Norte) (a,b) = (toEnum a+0.7,toEnum b+0.7)
--centroPeca (Curva Este) (a,b) = (toEnum a+0.3,toEnum b+0.7)
--centroPeca (Curva Sul) (a,b) = (toEnum a+0.3,toEnum b+0.3)
--centroPeca (Curva Oeste) (a,b) = (toEnum a+0.7,toEnum b+0.3)
--centroPeca _ (a,b) = (toEnum a+0.5,toEnum b+0.5)

estadoInicial :: Jogo -> Float -> [(String,Picture)] -> Estado
estadoInicial m tam imgs = Estado { jogo = m
--                                  , acoes = replicate njogadores parado
--                                  , timer = 0
                                  , tamBloco = tam
                                  , batotas = replicate (length (carros m)) 0
                                  , mortes = replicate (length (carros m)) 0
                                  , imagens = imgs }

qntnitro = 5


carroInicial :: Tabuleiro -> Posicao -> Int -> Carro
carroInicial t (a,b) i = Carro { posicao    = centroPeca tp (a,b)
                               , direcao    = 0
                               , velocidade = (0,0)
                               }
    where  (Peca tp _) = (atNote2 "carroInicial" t b a)


atualizaEMovimenta :: Tempo -> Jogo -> [Acao] -> (Jogo,[Float],[Float])
atualizaEMovimenta t j a = (jogoT4 { carros = map (\(i,_,_) -> i) carrosT3 }
                           , map (\(_,i,_) -> i) carrosT3
                           , map (\(_,_,i) -> i) carrosT3)
  where jogadores = length (carros j) - 1
        jogoT4    = geraJogoT4 jogadores
        carrosT3  = map movimentaT3 [0..jogadores]
        geraJogoT4 n | n<0  = j
                     | n>=0 = atualiza t (geraJogoT4 (n-1)) n (atNote "atualiza" a n)
        Mapa _ tab = mapa jogoT4
        movimentaT3 i | c == Nothing = (l, 1.5, 0)
                      | batota (mapa jogoT4) hi ci = (l', 0, 1.5)
                      | otherwise = (fromJust c, 0, 0)
          where c = colide tab t ci
                l = lixa (mapa jogoT4) hi ci
                l' = lixa (mapa jogoT4) (tail hi) ci
                hi = (atNote "hist" (historico jogoT4) i)
                ci = (atNote "carr" (carros jogoT4) i)

batota :: Mapa -> [Posicao] -> Carro -> Bool
batota (Mapa p0 m) (h1:h2:_) _ = (length whereWasI - length whereAmI) > 4
    where prc = percorre [] m (fst p0) (snd p0)
          whereAmI  = dropWhile (\(_,i,_) -> i /= h1) (prc++prc)
          whereWasI = dropWhile (\(_,i,_) -> i /= h2) (prc++prc)
batota _ _ _ = False

lixa :: Mapa -> [Posicao] -> Carro -> Carro
lixa (Mapa _ t) ((i,j):_) c = c { posicao = p , velocidade = (0,0) }
  where Peca tp _ = atNote2 "lixa" t j i 
        p = centroPeca tp (i,j)

--- begin gloss ---

getImage x e = fromJust $ lookup x (imagens e)

glossDesenha :: Bool -> Estado -> [Acao] -> Picture
glossDesenha drawMapa e acoes = Pictures 
                [Translate (-toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) (Pictures (m'++meta:p))]
--                ,Translate ((-100)+toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) barra]
     where (Mapa ((i,j),_) m) = mapa (jogo e)
           m' = if drawMapa then glossMapa e (0,0) m else [Blank]
--           barra = barraEstado e
           p = map (glossCarro e acoes) [0..length (jogadores $ jogo e) - 1] -- [0..3]
           meta = Color green $ Line [((toEnum i*tamBloco e),-(toEnum j*tamBloco e))
                                     ,((toEnum i*tamBloco e),-(toEnum j*tamBloco e)-tamBloco e)]
           x = (length (head m))
           y = (length m)


glossBloco :: Estado -> Peca -> Picture 
glossBloco e (Peca Recta p)          = Color (corAltura p) $ Polygon [(0,-tamBloco e),(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]
glossBloco e (Peca (Rampa Norte) p)  = transitaBloco e (corAltura (p+1),corAltura p) False
glossBloco e (Peca (Rampa Oeste) p)  = transitaBloco e (corAltura (p+1),corAltura p) True
glossBloco e (Peca (Rampa Sul) p)    = transitaBloco e (corAltura p,corAltura (p+1)) False
glossBloco e (Peca (Rampa Este) p)   = transitaBloco e (corAltura p,corAltura (p+1)) True
glossBloco e (Peca (Curva Oeste) p)  = Pictures [getImage "lava" e,Color (corAltura p) $ Polygon [(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]]
glossBloco e (Peca (Curva Sul) p)    = Pictures [getImage "lava" e,Color (corAltura p) $ Polygon [(0,-tamBloco e),(tamBloco e,0),(0,0)]]
glossBloco e (Peca (Curva Norte) p)  = Pictures [getImage "lava" e,Color (corAltura p) $ Polygon [(0,-tamBloco e),(tamBloco e,-tamBloco e),(tamBloco e,0)]]
glossBloco e (Peca (Curva Este) p)   = Pictures [getImage "lava" e,Color (corAltura p) $ Polygon [(0,0),(0,-tamBloco e),(tamBloco e,-tamBloco e)]]
glossBloco e (Peca Lava _)  = getImage "lava" e

numalturas = 5
corAltura :: Altura -> Color
corAltura a | a >= 0 = makeColor c c c 1
    where c = (toEnum a) * 1 / numalturas
corAltura a | a < 0  = makeColor r 0 0 1
    where r = abs (toEnum a) * 1 / numalturas

transitaBloco :: Estado -> (Color,Color) -> Bool -> Picture
transitaBloco e (c1,c2) i = Translate 0 gy $ Rotate g $ Pictures [a,b,c]
  where a = Color c1 $ Polygon [(0,0),(tamBloco e/2,-tamBloco e),(tamBloco e,0)]
        b = Color c2 $ Polygon [(0,0),(tamBloco e/2,-tamBloco e),(0,-tamBloco e)]
        c = Color c2 $ Polygon [(tamBloco e,0),(tamBloco e/2,-tamBloco e),(tamBloco e,-tamBloco e)]
        g = if i then -90 else 0
        gy = if i then -tamBloco e else 0

glossMapa :: Estado -> (Float,Float) -> Tabuleiro -> [Picture]
glossMapa e (x,y) [] = []
glossMapa e (x,y) ([]:ls) = glossMapa e (0,y-tamBloco e) ls
glossMapa e (x,y) ((c:cs):ls) = (Translate x y $ glossBloco e c) : glossMapa e (x+tamBloco e,y) (cs:ls)

glossCarro :: Estado -> [Acao] -> Int -> Picture
glossCarro s acoes i = Pictures [Translate (fromx) (fromy) $ Scale 0.5 0.5 $ Rotate (-double2Float a) (Pictures [nits,pic,mrt,btt]),vel]
     where p@(x,y) = posicao ci
           v = velocidade ci
           a = direcao ci
           pic = getImage ("c"++(show (i+1))) s
           nitpic i = getImage ("nitro" ++ show (i+1)) s
           pufpic = getImage "puff" s
           Mapa _ m = mapa (jogo s)
           t = atNote2 "glossCarro" m (floor y) (floor x) 
           isnit = isTarget s acoes i
           nits = Pictures $ map nit isnit
           nit i | ni > 0 = Translate (-tamBloco s / 3) 0 (nitpic i)
                 | ni <= 0 = Translate (-tamBloco s / 3) 0 pufpic
                 | otherwise = Blank
           (x',y') = p .+. v
           fromx = realToFrac x * tamBloco s
           fromy = - realToFrac y * tamBloco s
           tox = realToFrac x' * tamBloco s
           toy = -realToFrac y' * tamBloco s
           vel = Color yellow $ Line [(fromx,fromy),(tox,toy)]
           mrt = if (atNote "glossMor" (mortes s) i > 0) then Scale 2 2 (Translate (-tamBloco s / 2) (tamBloco s / 2) (getImage "mrt" s)) else Blank
           btt = if (atNote "glossBat" (batotas s) i > 0) then Scale 2 2 (Translate (-tamBloco s / 2) (tamBloco s / 2) (getImage "btt" s)) else Blank
           ci = atNote "glossCarr" (carros $ jogo s) i
           ni = atNote "glossNitr" (nitros $ jogo s) i


isTarget :: Estado -> [Acao] -> Int -> [Int]
isTarget e acoes i = map fst (filter (\(x,y) -> y == Just i) as)
    where as = zip [0..] (map nitro acoes)


glossTempo :: Float -> Estado -> [Acao] -> Estado
glossTempo t m acoes = m { jogo = j'
                         , batotas = (zipWith (\a b -> (max a b)-t) (batotas m) btts)
                         , mortes = (zipWith (\a b -> (max a b)-t) (mortes m) mrts)
                         }
  where (j',mrts,btts) = atualizaEMovimenta (float2Double t) (jogo m) acoes

-- end gloss --

joga :: IO ()
joga = do    
        screen@(Display screenx screeny) <- getDisplay 
        
        txt <- CW.getTextContent
        (tempo,jog,acao) <- case readMaybe txt :: Maybe (Tempo,Jogo,Acao) of
            Nothing -> error $ "erro ao carregar (Tempo,Jogo,Acao): " ++ show txt
            Just (tempo,jog,acao) -> return (tempo,jog,acao)
        unless (validaJogo' tempo jog) $ error $ "teste (Tempo,Jogo,Acao) inválido: " ++ show (tempo,jog,acao)
        let m@(Mapa _ tab) = mapa jog
        let x::Int = length (head tab)
            y::Int = length tab
            tamanhoX::Float = (realToFrac screenx) / (realToFrac x)
            tamanhoY::Float = (realToFrac screeny) / (realToFrac y)
            tamanho = min tamanhoX tamanhoY
            s = tamanho / 100
        imgs <- loadImages tamanho screen
        let e1 = estadoInicial jog tamanho imgs
        let acoes = replicate (length $ carros jog) acao --replicate i parado ++ [acao] ++ replicate (3-i) parado
        let e2 = glossTempo (realToFrac tempo) e1 acoes
        let p1 = glossDesenha True e1 acoes
        let p2 = glossDesenha False e2 []
        display screen back (Pictures [p1,p2])
  where
    --screenx = 1024
    --screeny = 768
    --screen = (InWindow "MicroMachines" (screenx,screeny) (0, 0))
    back = greyN 0.5 

main = catch (joga) $ \(e::SomeException) -> CW.trace (Text.pack $ displayException e) $ throw e

--main = do
--    joga teste 0

--teste :: (Tempo,Jogo,Acao)
--teste = (0.4,j',acao)
--    where
--    mapas = [Mapa ((3,1),Este) testeMapa]
--    ini@(Mapa p m) = mapas!!0
--    acao = Acao True False True False (Just 0)
--    j = jogoInicial ini
--    j' = j { carros = ((head $ carros j) { velocidade = (1,0)}) : tail (carros j) }

dir :: Ponto -> (Peca,Posicao,Orientacao) -> Double
dir p0 (Peca t _,p,_) = snd $ componentsToArrow (p'.-.p0)
  where p' = centroPeca t p

distRad :: Int -> Int -> Int
distRad r1 r2 = ((r2-r1) + 180) `mod` 360 - 180

--atNote2 str xys x y = atNote str (atNote str xys x) y