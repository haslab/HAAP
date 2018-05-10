{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import JSImages
import LI11718
import OracleT1

import System.Environment

import Data.Maybe
import Data.List as List
import qualified Data.Text as Text
import Text.Read

import Control.Monad
import Control.Exception

import GHC.Float

import qualified CodeWorld as CW
import Graphics.Gloss hiding ((.*.),(.+.),(.-.))
--import Graphics.Gloss.Data.Picture          
--import Graphics.Gloss.Interface.Pure.Game   
--import Graphics.Gloss.Juicy
--import Graphics.Gloss.Geometry.Line

import Safe

data EstadoJogo = J { mapaE      :: Either Mapa Tabuleiro
                    , terreno   :: Terreno
                    , tamBloco :: Float
                    , imagens :: [(String,Picture)]
                    }

data Terreno = T { delta_roda    :: Double
                 , delta_acel    :: Double    
                 , delta_drag    :: Double   
                 , delta_drift   :: Double  
                 , delta_gravity :: Double
                 }

-- copiado do Gloss para Double
closestPosicaoOnL :: Ponto -> Ponto -> Ponto -> Double
closestPosicaoOnL p1 p2 p3 = (p3 .-. p1) .$. (p2 .-. p1) / (p2 .-. p1) .$. (p2 .-. p1)

(.*.) :: Double -> (Double,Double) -> (Double,Double)
(.*.) x (a,b) = ((x*a),(x*b))
       
(.+.) :: (Double,Double) -> (Double,Double) -> (Double,Double)
(.+.) (x,y) (a,b) = ((x+a),(y+b))

(.-.) :: (Double,Double) -> (Double,Double) -> (Double,Double)
(.-.) (x,y) (a,b) = ((x-a),(y-b))

-- the dot product between two (Double,Double)s
(.$.) :: (Double,Double) -> (Double,Double) -> Double
(.$.) (d1,a1) (d2,a2) = (x1*x2) + (y1*y2)
    where (x1,y1) = (d1,a1)
          (x2,y2) = (d2,a2)

radians th = th * (pi/180)
degrees th = th * (180/pi)

arrowToComponents :: (Double,Double) -> Ponto
arrowToComponents (v,th) = (getX v th,getY v th)
    where getX v th = v * cos (radians (th))
          getY v th = v * sin (radians (-th))

componentsToArrow :: Ponto -> (Double,Double)
componentsToArrow (x,0) | x >= 0 = (x,0)
componentsToArrow (x,0) | x < 0 = (x,180)
componentsToArrow (0,y) | y >= 0 = (y,-90)
componentsToArrow (0,y) | y < 0 = (y,90)
componentsToArrow (x,y) = (hyp,dir angle)
    where
    dir o = case (x >= 0, y >= 0) of
                (True,True) -> -o
                (True,False) -> o
                (False,False) -> 180 - o
                (False,True) -> 180 + o
    hyp = sqrt ((abs x)^2 + (abs y)^2)
    angle = degrees $ atan (abs y / abs x)

dist :: Ponto -> Ponto -> Double
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2+(y2-y1)^2)

-- gloss


glossBloco :: EstadoJogo -> Peca -> Picture 
glossBloco e (Peca Recta p)          = Color (corAltura p) $ Polygon [(0,-tamBloco e),(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]
--glossBloco e (Peca Recta p)          = getImage "recta" e --Color (corAltura p) $ Polygon [(0,-tamBloco e),(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]
glossBloco e (Peca (Rampa Norte) p)  = transitaBloco e (corAltura (p+1),corAltura p) False
glossBloco e (Peca (Rampa Oeste) p)  = transitaBloco e (corAltura (p+1),corAltura p) True
glossBloco e (Peca (Rampa Sul) p)    = transitaBloco e (corAltura p,corAltura (p+1)) False
glossBloco e (Peca (Rampa Este) p)   = transitaBloco e (corAltura p,corAltura (p+1)) True
glossBloco e (Peca (Curva Oeste) p)  = Pictures [getImage "lava" e,Color (corAltura p) $ Polygon [(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]]
glossBloco e (Peca (Curva Sul) p)    = Pictures [getImage "lava" e,Color (corAltura p) $ Polygon [(0,-tamBloco e),(tamBloco e,0),(0,0)]]
glossBloco e (Peca (Curva Norte) p)  = Pictures [getImage "lava" e,Color (corAltura p) $ Polygon [(0,-tamBloco e),(tamBloco e,-tamBloco e),(tamBloco e,0)]]
glossBloco e (Peca (Curva Este) p)   = Pictures [getImage "lava" e,Color (corAltura p) $ Polygon [(0,0),(0,-tamBloco e),(tamBloco e,-tamBloco e)]]
--glossBloco e (Peca (Curva Oeste) p)  = Rotate 270 $ getImage "curva" e --Color (corAltura p) $ Polygon [(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]
--glossBloco e (Peca (Curva Sul) p)    = Rotate 180 $ getImage "curva" e --Color (corAltura p) $ Polygon [(0,-tamBloco e),(tamBloco e,0),(0,0)]
--glossBloco e (Peca (Curva Norte) p)  = Rotate 0 $ getImage "curva" e --Color (corAltura p) $ Polygon [(0,-tamBloco e),(tamBloco e,-tamBloco e),(tamBloco e,0)]
--glossBloco e (Peca (Curva Este) p)   = Rotate 90 $ getImage "curva" e --Color (corAltura p) $ Polygon [(0,0),(0,-tamBloco e),(tamBloco e,-tamBloco e)]
glossBloco e (Peca Lava _)  = getImage "lava" e--Blank

numalturas = 5
corAltura :: Altura -> Color
corAltura a | a >= 0 = makeColor c c c 1
    where c = (toEnum a) * 1 / numalturas
corAltura a | a < 0  = makeColor r 0 0 1
    where r = abs (toEnum a) * 1 / numalturas

--makeColor (0.2*(toEnum a+2)) (0.2*(toEnum a+2)) (0.15*(toEnum a+2)) 1

transitaBloco :: EstadoJogo -> (Color,Color) -> Bool -> Picture
transitaBloco e (c1,c2) i = Translate 0 gy $ Rotate g $ Pictures [a,b,c]
  where a = Color c1 $ Polygon [(0,0),(tamBloco e/2,-tamBloco e),(tamBloco e,0)]
        b = Color c2 $ Polygon [(0,0),(tamBloco e/2,-tamBloco e),(0,-tamBloco e)]
        c = Color c2 $ Polygon [(tamBloco e,0),(tamBloco e/2,-tamBloco e),(tamBloco e,-tamBloco e)]
        g = if i then -90 else 0
        gy = if i then -tamBloco e else 0

glossMapa :: EstadoJogo -> (Float,Float) -> [[Peca]] -> [Picture]
glossMapa e (x,y) [] = []
glossMapa e (x,y) ([]:ls) = glossMapa e (0,y-tamBloco e) ls
--glossMapa e (x,y) ((c:cs):ls) = (Translate (x+(tamBloco e / 2)) (y-(tamBloco e / 2)) $ glossBloco e c) : glossMapa e (x+tamBloco e,y) (cs:ls)
glossMapa e (x,y) ((c:cs):ls) = (Translate x y $ glossBloco e c) : glossMapa e (x+tamBloco e,y) (cs:ls)

glossDesenha :: EstadoJogo -> Picture
glossDesenha e = Translate (-toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) $ Pictures (m'++[meta])
    where
    tab = either (\(Mapa _ m) -> m) id $ mapaE e
    m' = glossMapa e (0,0) tab
    meta = case mapaE e of
            Left (Mapa ((i,j),dir_ori) m) ->
                let meta_ori = metaOri (atNote2 "meta" m j i) dir_ori
                in Color green $ Line $ metaLine meta_ori (realToFrac i) (realToFrac j) (tamBloco e)
            Right tab -> Blank
    x = (length (head tab))
    y = (length tab)

estadoInicial :: Either Mapa Tabuleiro -> Float -> [(String,Picture)] -> EstadoJogo
estadoInicial m tam imgs = J { mapaE = m 
                                        , terreno = standard
                                        , tamBloco = tam
                                        , imagens = imgs
                                        }

standard :: Terreno
standard = T { delta_roda = 90     -- rotaçao, angulo / segundo
             , delta_acel = 4      -- aceleraçao, velocidade / segundo 
             , delta_drag = 1      -- abrandamento, velocidade / segundo 
             , delta_drift = 8     -- resistencia pneus, velocidade / segundo 
             , delta_gravity = 3.5 -- gravidade, velocidade / segundo 
             }

posInicial :: Posicao -> Ponto
posInicial (a,b) = (toEnum a+0.5,toEnum b+0.5)

carroInicial :: Posicao -> Int -> Carro
carroInicial (a,b) i = Carro { posicao   = posInicial (a,b)
                         , direcao    = 0
                         , velocidade = (0,0)
                         }

theFloorIsLava :: Dimensao -> [[Peca]]
theFloorIsLava (n,m) = replicate m (replicate n (Peca Lava altLava)) 

getImage x e = fromJust $ List.lookup x (imagens e)

joga :: IO ()
joga = do    
    screen@(Display screenx screeny) <- getDisplay
    txt <- CW.getTextContent
    mapa <- case readMaybe txt :: Maybe Mapa of
        Nothing -> case readMaybe txt :: Maybe Caminho of
            Nothing -> case readMaybe txt :: Maybe Tabuleiro of
                Nothing -> error $ "erro ao carregar caminho, mapa ou tabuleiro: " ++ show txt
                Just tab -> return $ Right tab
            Just caminho -> return $ Left $ constroi caminho
        Just m -> return $ Left m
    let tab = either (\(Mapa _ tab) -> tab) id mapa
    let back = greyN 0.5 
        x = toEnum (maybe 0 length (headMay tab))
        y = toEnum (length tab)
        tamanhoX::Float = (realToFrac screenx) / (realToFrac x)
        tamanhoY::Float = (realToFrac screeny) / (realToFrac y)
        tamanho = min tamanhoX tamanhoY
    imgs <- loadImages tamanho screen
    let e = (estadoInicial mapa tamanho imgs)
    display screen back (glossDesenha e)

main = catch joga $ \(e::SomeException) -> CW.trace (Text.pack $ displayException e) $ throw e

