{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import JSImages
import LI11718
import OracleT1 (atNote2)
import qualified OracleT1 as T1
import qualified OracleT3 as T3
import qualified OracleT2 as T2

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

ponto2Pos :: Ponto -> Posicao
ponto2Pos (x,y) = (i,j)
    where x' = floor x
          y' = floor y
          i = x'
          j = y'

data EstadoJogo = J { mapaE      :: Tabuleiro
                    , terreno   :: Terreno
                    , tamBloco :: Float
                    , imagens :: [(String,Picture)]
                    }

data EstadoCarro = C { posicaoC    :: (Double,Double)
                     , direcaoC    :: Double
                     , velocidadeC :: (Double,Double) 





                     , checkPonto :: Int

                     , acelera    :: Bool
                     , trava      :: Bool
                     , viraD      :: Bool
                     , viraE      :: Bool
                     }

data Terreno = T { delta_roda    :: Double
                 , delta_acel    :: Double    
                 , delta_drag    :: Double   
                 , delta_drift   :: Double  
                 , delta_gravity :: Double
                 }

estadoInicial :: Tabuleiro -> Float -> [(String,Picture)] -> EstadoJogo
estadoInicial t tam imgs = J { mapaE = t
                                        , terreno = standard
                                        , tamBloco = tam
                                        , imagens = imgs
                                        }

posInicial :: Tabuleiro -> Posicao -> Ponto
posInicial t (a,b) = T2.centroPeca tp (a,b)
  where (Peca tp _) = (atNote2 "posInicial" t b a)

carroInicial :: Tabuleiro -> Posicao -> Int -> EstadoCarro
carroInicial t (a,b) i = C { posicaoC    = posInicial t (a,b)
                           , direcaoC    = 0
                           , velocidadeC = (0,0)
                           , checkPonto = 0
       
                           , acelera    = False
                           , trava      = False
                           , viraD      = False
                           , viraE      = False
                           }

standard :: Terreno
standard = T { delta_roda = 180     -- rotaçao, angulo / segundo
             , delta_acel = 4      -- aceleraçao, velocidade / segundo 
             , delta_drag = 1      -- abrandamento, velocidade / segundo 
             , delta_drift = 8     -- resistencia pneus, velocidade / segundo 
             , delta_gravity = 3.5 -- gravidade, velocidade / segundo 
             }

--centroPeca :: Tipo -> Posicao -> Ponto
--centroPeca (Curva Norte) (a,b) = (toEnum a+0.7,toEnum b+0.7)
--centroPeca (Curva Este) (a,b) = (toEnum a+0.3,toEnum b+0.7)
--centroPeca (Curva Sul) (a,b) = (toEnum a+0.3,toEnum b+0.3)
--centroPeca (Curva Oeste) (a,b) = (toEnum a+0.7,toEnum b+0.3)
--centroPeca _ (a,b) = (toEnum a+0.5,toEnum b+0.5)

-- geometry

-- copiado do Gloss para Double
intersecta :: (Ponto,Ponto) -> (Ponto,Ponto) -> Maybe Ponto
intersecta (p1,p2) (p3,p4) | Just p0 <- intersectaL p1 p2 p3 p4
                           , t12   <- closestPontoOnL p1 p2 p0
                           , t23   <- closestPontoOnL p3 p4 p0
                           , t12 >= 0 && t12 <= 1
                           , t23 >= 0 && t23 <= 1
                           = Just p0
                           | otherwise
                           = Nothing

-- copiado do Gloss para Double
intersectaL :: Ponto -> Ponto -> Ponto -> Ponto -> Maybe Ponto
intersectaL (x1, y1) (x2, y2) (x3, y3) (x4, y4)
 = let  dx12  = x1 - x2
        dx34  = x3 - x4
        dy12  = y1 - y2
        dy34  = y3 - y4
  
        den = dx12 * dy34  - dy12 * dx34

    in if den == 0
       then Nothing
       else let det12 = x1*y2 - y1*x2 
                det34 = x3*y4 - y3*x4 
                numx  = det12 * dx34 - dx12 * det34
                numy  = det12 * dy34 - dy12 * det34
            in Just (numx / den, numy / den)

-- copiado do Gloss para Double
closestPontoOnL :: Ponto -> Ponto -> Ponto -> Double
closestPontoOnL p1 p2 p3 = (p3 .-. p1) .$. (p2 .-. p1) / (p2 .-. p1) .$. (p2 .-. p1)

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

glossMapa :: EstadoJogo -> (Float,Float) -> Tabuleiro -> [Picture]
glossMapa e (x,y) [] = []
glossMapa e (x,y) ([]:ls) = glossMapa e (0,y-tamBloco e) ls
--glossMapa e (x,y) ((c:cs):ls) = (Translate (x+(tamBloco e / 2)) (y-(tamBloco e / 2)) $ glossBloco e c) : glossMapa e (x+tamBloco e,y) (cs:ls)
glossMapa e (x,y) ((c:cs):ls) = (Translate x y $ glossBloco e c) : glossMapa e (x+tamBloco e,y) (cs:ls)

desenhaBounces :: Display -> Float -> ([Ponto],Maybe Velocidade) -> Picture
desenhaBounces (Display sx sy) t ([x],Nothing) = Pictures [Scale 0.3 0.3 $ Color green $ Translate (realToFrac sx / 2) (- realToFrac sy / 2) $ Text "Out of Bounds",a]
  where a = Color green $ Translate (fst $ norm x) (snd $ norm x) $ ThickCircle 0 10
        norm (x,y) = (t * double2Float x, -t * double2Float y)
desenhaBounces screen t (l,v) = Pictures [i,a,b]
  where i = Color green $ Line $ map norm l
        a = Color blue $ Translate (fst $ norm $ head l) (snd $ norm $ head l) $ ThickCircle 0 10
        b = Color morte $ Translate (fst $ norm $ last l) (snd $ norm $ last l) $ ThickCircle 0 10
        norm (x,y) = (t * double2Float x, -t * double2Float y)
        morte = if isJust v then green else red

glossDesenhaBounces :: Display -> [Carro] -> ([Ponto],Maybe Velocidade) -> EstadoJogo -> Picture
glossDesenhaBounces screen carros ps e = Translate (-toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) $ Pictures (m'++desenhaCarros e carros++[desenhaBounces screen (tamBloco e) ps])
     where m = mapaE e
           m' = glossMapa e (0,0) m
           x = (length (head m))
           y = (length m)

desenhaCarros :: EstadoJogo -> [Carro] -> [Picture]
desenhaCarros e = concatMap (desenhaCarro e)

desenhaCarro :: EstadoJogo -> Carro -> [Picture]
desenhaCarro e (Carro p@(x,y) a v) = [Translate fromx fromy $ Scale 0.5 0.5 $ Rotate (-double2Float a) carro,vel]
    where
    fromx = (double2Float x*tamBloco e)
    fromy = (-double2Float y*tamBloco e)
    carro = getImage "c1" e
    (x',y') = p .+. v
    tox = double2Float x' * tamBloco e
    toy = -double2Float y' * tamBloco e
    vel = Color yellow $ Line [(fromx,fromy),(tox,toy)]

getImage x e = fromJust $ List.lookup x (imagens e)

joga :: IO ()
joga = do   
    screen@(Display screenx screeny) <- getDisplay 
    txt <- CW.getTextContent
    (tab,tempo,carro) <- case readMaybe txt :: Maybe (Tabuleiro,Tempo,Carro) of
        Nothing -> case readMaybe txt :: Maybe (Tempo,Carro) of
            Nothing -> error $ "erro ao carregar (Tabuleiro,Tempo,Carro) ou (Tempo,Carro): " ++ show txt
            Just (tempo,carro) -> let (Mapa _ tab) = m_ex1 in return (tab,tempo,carro)
        Just (tab,tempo,carro) -> return (tab,tempo,carro)
    let from = posicao carro
        to = posicao carro .+. (tempo .*. velocidade carro)
    let back = greyN 0.5 
        x = toEnum (length (head tab))
        y = toEnum (length tab)
        tamanhoX::Float = (realToFrac screenx) / (realToFrac x)
        tamanhoY::Float = (realToFrac screeny) / (realToFrac y)
        tamanho = min tamanhoX tamanhoY
    let imgs = makeImages tamanho screen
    let e = (estadoInicial tab tamanho imgs)
        bs = if (T2.validaPonto from tab && T2.validaTabuleiro tab)
             then T3.colideLocalAcc [] tab (1,1) (from,to) (ponto2Pos from)
             else ([from],Nothing)
        carro' = if T2.validaTabuleiro tab then T3.colide tab tempo carro else Nothing
        carros = filter (flip T2.validaPonto tab . posicao) (carro:maybeToList carro')
    display screen back (glossDesenhaBounces screen carros bs e)
    --play screen back e glossDesenha glossEvento glossTempo

main = catch (joga) $ \(e::SomeException) -> CW.trace (Text.pack $ displayException e) $ throw e

m_ex1 = T1.constroi c_ex1

c_ex1 :: Caminho
c_ex1 = [Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Desce,Avanca,CurvaEsq,CurvaDir
      ,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaEsq,Avanca,Avanca
      ,Desce,CurvaDir,CurvaDir,Avanca,Avanca,Desce,CurvaEsq,CurvaDir,Sobe,CurvaDir
      ,CurvaEsq,CurvaDir,CurvaEsq,Avanca,CurvaDir,Sobe,Sobe,Avanca,Avanca,CurvaDir,Avanca]