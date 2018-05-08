{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import JSImages
import LI11718
import qualified Tarefa3_2017li1g183 as T3

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

ponto2Pos :: Ponto -> Posicao
ponto2Pos (x,y) = (i,j)
    where x' = floor x
          y' = floor y
          i = x'
          j = y'

data EstadoJogo = J { mapaE      :: Mapa
                    , terreno   :: Terreno
                    , jogador :: EstadoCarro
                    , tamBloco :: Float
                    , imagens :: [(String,Picture)]
                    }

data EstadoCarro = C { posicaoC    :: (Double,Double)
                     , direcaoC    :: Double
                     , velocidadeC :: (Double,Double) 





                     , checkPosicao :: Int

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

estadoInicial :: Mapa -> Float -> [(String,Picture)] -> EstadoJogo
estadoInicial m@(Mapa p t) tam imgs = J { mapaE = m 
                                        , terreno = standard
                                        , jogador = carroInicial t (fst p) 0
                                        , tamBloco = tam
                                        , imagens = imgs
                                        }

posInicial :: Tabuleiro -> Posicao -> Ponto
posInicial t (a,b) = centroPeca tp (a,b)
  where (Peca tp _) = (atNote2 "posInicial" t b a)

carroInicial :: Tabuleiro -> Posicao -> Int -> EstadoCarro
carroInicial t (a,b) i = C { posicaoC    = posInicial t (a,b)
                           , direcaoC    = 0
                           , velocidadeC = (0,0)
                           , checkPosicao = 0
       
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

move :: Double -> EstadoJogo -> EstadoJogo
move n e = e { jogador = c' }
  where p = (jogador e)
        Mapa _ m = (mapaE e)
        c' = moveCarro e n (terreno e) p

moveCarro :: EstadoJogo -> Double -> Terreno -> EstadoCarro -> EstadoCarro
moveCarro b n t c = andaCarro b n t $ rodaCarro n t c

rodaCarro :: Double -> Terreno -> EstadoCarro -> EstadoCarro
rodaCarro t r c | viraD c = c { direcaoC = direcaoC c - (t*delta_roda r)}
                | viraE c = c { direcaoC = direcaoC c + (t*delta_roda r)}
                | otherwise = c

andaCarro :: EstadoJogo -> Tempo -> Terreno -> EstadoCarro -> EstadoCarro
andaCarro e t r c = maybe (c { posicaoC = posInicial m (fst p0), velocidadeC = (0,0) }) id col
  where v' = (velocidadeC c) .+. (t .*. ((accelVec r c) .+. (dragVec r c) .+. (driftVec r c) .+. (gravityVec r b c)))
        Mapa p0 m = mapaE e
        (i,j) = (ponto2Pos (posicaoC c))
        b = atNote2 "andaCarro" m j i
        c' = c { velocidadeC = v' }
        col = colideEstado (mapaE e) t c'
        (vv,va) = componentsToArrow (velocidadeC c)

colideEstado :: Mapa -> Tempo -> EstadoCarro -> Maybe EstadoCarro
colideEstado (Mapa _ tab) tempo e = do
    let carro = Carro (posicaoC e) (direcaoC e) (velocidadeC e)
    carro' <- T3.movimenta tab tempo carro
    return $ e { posicaoC = posicao carro', direcaoC = direcao carro', velocidadeC = velocidade carro' }

accelVec :: Terreno -> EstadoCarro -> (Double,Double)
accelVec t c | acelera c && not (trava c) = arrowToComponents (delta_acel t,direcaoC c)
             | trava c && not (acelera c) = arrowToComponents (delta_acel t,direcaoC c + 180)
             | otherwise = (0,0)

dragVec :: Terreno -> EstadoCarro -> (Double,Double)
dragVec t c = arrowToComponents (delta_drag t*v,a + 180) 
  where (v,a) = componentsToArrow (velocidadeC c)

driftVec :: Terreno -> EstadoCarro -> (Double,Double)
driftVec t c = arrowToComponents (driftCoef,driftAngle)
  where (vv,av) = componentsToArrow $ velocidadeC c
        ad = direcaoC c
        driftAngle = if (sin (radians (av-ad))) > 0
                     then ad-90 -- going right 
                     else ad+90 -- going left
        driftCoef = vv * delta_drift t * abs (sin (radians (av-ad)))
    
gravityVec :: Terreno -> Peca -> EstadoCarro -> (Double,Double)
gravityVec t (Peca (Rampa Sul) _) c = arrowToComponents (delta_gravity t, 90)
gravityVec t (Peca (Rampa Norte) _) c = arrowToComponents (delta_gravity t, 270)
gravityVec t (Peca (Rampa Oeste) _) c = arrowToComponents (delta_gravity t, 0)
gravityVec t (Peca (Rampa Este) _) c = arrowToComponents (delta_gravity t, 180)
gravityVec t _ c = (0,0)

centroPeca :: Tipo -> Posicao -> Ponto
centroPeca (Curva Norte) (a,b) = (toEnum a+0.7,toEnum b+0.7)
centroPeca (Curva Este) (a,b) = (toEnum a+0.3,toEnum b+0.7)
centroPeca (Curva Sul) (a,b) = (toEnum a+0.3,toEnum b+0.3)
centroPeca (Curva Oeste) (a,b) = (toEnum a+0.7,toEnum b+0.3)
centroPeca _ (a,b) = (toEnum a+0.5,toEnum b+0.5)

-- geometry

-- copiado do Gloss para Double
intersecta :: (Ponto,Ponto) -> (Ponto,Ponto) -> Maybe Ponto
intersecta (p1,p2) (p3,p4) | Just p0 <- intersectaL p1 p2 p3 p4
                           , t12   <- closestPosicaoOnL p1 p2 p0
                           , t23   <- closestPosicaoOnL p3 p4 p0
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

glossCarro :: EstadoJogo -> Picture
glossCarro s = Translate (double2Float x*tamBloco s) (-double2Float y*tamBloco s) $ Scale 0.5 0.5 $ Rotate (-double2Float a) pic
     where (x,y) = posicaoC (jogador s)
           a = direcaoC (jogador s)
           pic = getImage "c1" s --Polygon [(-6,5),(6,0),(-6,-5)]
           Mapa _ m = mapaE s
           t = atNote2 "glossCarro" m (floor y) (floor x) 

glossEvento :: Event -> EstadoJogo -> EstadoJogo
glossEvento e s = s { jogador = c' }
  where c' = glossEventoCarro e (jogador s)

glossEventoCarro :: Event -> EstadoCarro -> EstadoCarro
glossEventoCarro (EventKey (SpecialKey KeyDown ) kst _) e = e { trava   = kst == Down }
glossEventoCarro (EventKey (SpecialKey KeyUp   ) kst _) e = e { acelera = kst == Down }
glossEventoCarro (EventKey (SpecialKey KeyLeft ) kst _) e = e { viraE   = kst == Down }
glossEventoCarro (EventKey (SpecialKey KeyRight) kst _) e = e { viraD   = kst == Down }
glossEventoCarro _ st = st

glossTempo :: Float -> EstadoJogo -> EstadoJogo
glossTempo t m = move (float2Double t) m

glossDesenha :: EstadoJogo -> Picture
glossDesenha e = Translate (-toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) $ Pictures (m'++meta:[p])
     where (Mapa ((i,j),_) m) = mapaE e
           m' = glossMapa e (0,0) m
           p = glossCarro e
           meta = Color green $ Line [((toEnum i*tamBloco e),-(toEnum j*tamBloco e))
                                     ,((toEnum i*tamBloco e),-(toEnum j*tamBloco e)-tamBloco e)]
           x = (length (head m))
           y = (length m)

getImage x e = fromJust $ List.lookup x (imagens e)

joga :: Int -> IO ()
joga i = do   
    screen@(Display screenx screeny) <- getDisplay 
    let back = greyN 0.5 
        x = toEnum (length (head m))
        y = toEnum (length m)
        tamanhoX::Float = (realToFrac screenx) / (realToFrac x)
        tamanhoY::Float = (realToFrac screeny) / (realToFrac y)
        tamanho = min tamanhoX tamanhoY
        mapas = (map constroi caminhos_validos) ++ mapas_validos
        ini@(Mapa p m) = atNote "joga" mapas i
    let imgs = makeImages tamanho screen
    let e = (estadoInicial ini tamanho imgs)
    play screen back e glossDesenha glossEvento glossTempo

main = catch (joga 0) $ \(e::SomeException) -> CW.trace (Text.pack $ displayException e) $ throw e

-- exemplos

caminhos_validos, caminhos_invalidos :: [Caminho]
caminhos_validos   = [c_ex1,c_ex1',c_ex2,c_ex3,c_ex4,c_ex5,c_ex6]
caminhos_invalidos = [c_exOL,c_exOP,c_exDM,c_exHM,c_exR,c_exE]

mapas_validos, mapas_invalidos :: [Mapa]
mapas_validos   = [m_ex1,m_ex2,m_ex3]
mapas_invalidos = [m_exPI,m_exLV,m_exEX,m_exLH,m_why]

-- bom
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
m_ex1 = Mapa ((2,2),Este) [[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca (Curva Norte) 2,Peca (Curva Este) 2, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca (Curva Oeste) 2,Peca (Curva Sul) 2, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
                          ]

-- mapa nao geravel por caminhos, altura /= inicial sem possibilidade de rampas
m_ex2 = Mapa ((2,1),Este) [[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca (Curva Norte) 5,Peca (Curva Este) 5, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca (Curva Oeste) 5,Peca (Curva Sul) 5, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
                          ]

-- mapa minimo sem vizinhos
m_ex3 = Mapa ((2,1),Este) [[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca Recta 2,Peca Lava altLava,Peca Recta 2, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2, Peca Lava altLava]
                          ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
                          ]


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

-- caminho vazio
c_exE :: Caminho
c_exE = []

-- posicao inicial invalida
m_exPI = Mapa ((0,0),Este) [[Peca (Curva Norte) 2,Peca (Curva Este) 2],[Peca (Curva Oeste) 2,Peca (Curva Sul) 2]]

-- mapa so lava
m_exLV = Mapa ((0,0),Este) $ theFloorIsLava (5,10)

-- mapa com caminho extra
m_exEX = Mapa ((1,0),Este) [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Recta 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]

-- altura da lava invalida
m_exLH = Mapa ((1,0),Este) [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Lava 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]


m_why = Mapa ((1,1),Este) [[Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2],[Peca Recta 2,Peca Recta 2,Peca Recta 2],[Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2]]

-- T1

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
    where l = (take x (atNote "replace" m y)) ++ [e] ++ (drop (x+1) (atNote "replace" m y))

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

atNote2 str xys x y = atNote str (atNote str xys x) y
