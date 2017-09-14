{-# LANGUAGE ScopedTypeVariables #-}

import qualified CodeWorld as CW
import Graphics.Gloss hiding ((.*.))
import Graphics.Gloss.Data.Picture          
import Graphics.Gloss.Interface.Pure.Game   
--import Graphics.Gloss.Juicy
import Graphics.Gloss.Geometry.Line

import Text.Read


-- input da T1
type Caminho = [Passo]

data Passo = Segue | Direita  | Esquerda
           | Sobe  | Desce   
  deriving (Read,Show)

-- output da T1

type Mapa = [[Bloco]]
type Altura = Int 

data Bloco = PlanoVertical      Altura Int | PlanoHorizontal      Altura Int    
           | AltoBaixoVertical  Altura Int | AltoBaixoHorizontal  Altura Int     
           | BaixoAltoVertical  Altura Int | BaixoAltoHorizontal  Altura Int    
           | CurvaDireitaBaixo  Altura Int | CurvaEsquerdaBaixo   Altura Int 
           | CurvaDireitaCima   Altura Int | CurvaEsquerdaCima    Altura Int
           | Lava
  deriving (Show,Eq)

-- potencial assinatura da T1
escava :: Caminho -> Mapa
escava c = processa 0 c dirInit altInit posInit (theFloorIsLava 10)

-- estado inicial
type Direcao = Int

dirInit :: Direcao
dirInit = 0

posInit :: (Int,Int)
posInit = (2,2)

altInit :: Altura
altInit = 2

altMax :: Int
altMax = 5

dimensao :: Int
dimensao = 10

-- solucao

-- validações:
-- * dentro dos bounds do mapa (predefinidos)
-- * sem sobreposições
-- * percurso circular (posiçao, direçao, altura, terreno)
-- * altura e terreno dentro dos bounds

mexe :: (Int,Int) -> Direcao -> (Int,Int)
mexe (x,y) 0 = (x+1,y)
mexe (x,y) 1 = (x,y+1)
mexe (x,y) 2 = (x-1,y)
mexe (x,y) 3 = (x,y-1)

roda :: Direcao -> Bool -> Direcao
roda d True = (d+1) `mod` 4
roda d False = (d-1) `mod` 4

blocoRecto :: Int -> Direcao -> Altura -> Bloco
blocoRecto i 0 p = PlanoHorizontal p i
blocoRecto i 1 p = PlanoVertical   p i
blocoRecto i 2 p = PlanoHorizontal p i
blocoRecto i 3 p = PlanoVertical   p i

blocoCurvo :: Int -> Direcao -> Direcao -> Altura -> Bloco
blocoCurvo i 0 1 p = CurvaEsquerdaBaixo p i
blocoCurvo i 1 2 p = CurvaEsquerdaCima  p i
blocoCurvo i 2 3 p = CurvaDireitaCima   p i
blocoCurvo i 3 0 p = CurvaDireitaBaixo  p i
blocoCurvo i n m p = blocoCurvo i ((m+2)`mod`4) ((n+2)`mod`4) p

adapta :: Passo -> Altura -> Altura
adapta Sobe     a = a+1
adapta Desce    a = a-1
adapta _        a = a

forma :: Passo -> Bloco -> Bloco
forma Sobe      (PlanoVertical p i)    = AltoBaixoVertical p i
forma Sobe      (PlanoHorizontal p i)  = AltoBaixoHorizontal p i
forma Desce     (PlanoVertical p i)    = BaixoAltoVertical p i
forma Desce     (PlanoHorizontal p i)  = BaixoAltoHorizontal p i
forma _ b = b

processa :: Int -> Caminho -> Direcao -> Altura -> (Int,Int) -> Mapa -> Mapa
processa _ [] d p (x,y) m | (x,y) /= posInit || d /= dirInit || p /= altInit = []  -- não é circular
                          | otherwise = m
processa i (Direita:c) d p (x,y) m = processa (i+1) c d' p (mexe (x,y) d') m'
    where m' = replace m (x,y) (blocoCurvo i d d' p)
          d' = roda d True
processa i (Esquerda:c) d p (x,y) m = processa (i+1) c d' p (mexe (x,y) d') m'
    where m' = replace m (x,y) (blocoCurvo i d d' p)
          d' = roda d False
processa i (s:c) d p (x,y) m | p > altMax || p < 0 = []                           -- Alturas out of bounds
                             | otherwise = processa (i+1) c d p' (mexe (x,y) d) m'
    where m' = replace m (x,y) (forma (normaliza d s) (blocoRecto i d (min p p')))
          p' = adapta s p

normaliza d s | d > 1 = s
              | otherwise = inverte s

inverte Sobe      = Desce
inverte Desce     = Sobe
inverte s         = s

replace :: Mapa -> (Int,Int) -> Bloco -> Mapa
replace m (x,y) e | x < 0 || y < 0 || x >= length m || y >= length m = [] -- percurso out of bounds
                  | (m!!y!!x) /= Lava = []                                -- percurso com overlaps
                  | otherwise = (take y m) ++ [l] ++ (drop (y+1) m)
    where l = (take x (m!!y)) ++ [e] ++ (drop (x+1) (m!!y))

theFloorIsLava :: Int -> Mapa
theFloorIsLava n = replicate n (replicate n Lava) 

-- input da T2

data EstadoJogo = J { ecra      :: Display
                    , tamanhoBloco :: Int
                    , mapa      :: Mapa
                    , terreno   :: Terreno
                    , jogadores :: [EstadoCarro]
                    }

data EstadoCarro = C { posicao    :: (Float,Float)
                     , direcao    :: Float
                     , velocidade :: (Float,Float) 
                     , checkpoint :: Int

                     , acelera    :: Bool
                     , trava      :: Bool
                     , viraD      :: Bool
                     , viraE      :: Bool
                     }

data Terreno = T { delta_roda    :: Float
                 , delta_acel    :: Float    
                 , delta_drag    :: Float   
                 , delta_drift   :: Float  
                 , delta_gravity :: Float
                 }

estadoInicial :: Display -> Int -> Mapa -> EstadoJogo
estadoInicial screen tam m = J { ecra = screen, tamanhoBloco = tam, mapa = m 
                    , terreno = standard
                    , jogadores = map carroInicial [0..3]
                    }

carroInicial :: Int -> EstadoCarro
carroInicial i = C { posicao    = (2,1.7+(0.2*toEnum i))
                   , direcao    = 0
                   , velocidade = (0,0)
                   , checkpoint = 0
 
                   , acelera    = False
                   , trava      = False
                   , viraD      = False
                   , viraE      = False
                   }

standard :: Terreno
standard = T { delta_roda = 90   -- rotaçao, angulo / segundo
             , delta_acel = 4    -- aceleraçao, velocidade / segundo 
             , delta_drag = 1    -- abrandamento, velocidade / segundo 
             , delta_drift = 8   -- resistencia pneus, velocidade / segundo 
             , delta_gravity = 5 -- gravidade, velocidade / segundo 
             }

move :: Float -> Int -> EstadoJogo -> EstadoJogo
move n i e = e { jogadores = c':tail (jogadores e) }
  where p = (jogadores e!!i)
        b = (mapa e)!!(round $ snd $ posicao p)!!(round $ fst $ posicao p)
        c' = moveCarro b n (terreno e) p

moveCarro :: Bloco -> Float -> Terreno -> EstadoCarro -> EstadoCarro
moveCarro b n t c = andaCarro b n t $ rodaCarro n t c

rodaCarro :: Float -> Terreno -> EstadoCarro -> EstadoCarro
rodaCarro t r c | viraD c = c { direcao = direcao c - (t*delta_roda r)}
                | viraE c = c { direcao = direcao c + (t*delta_roda r)}
                | otherwise = c

(.*.) :: Float -> (Float,Float) -> (Float,Float)
(.*.) x (a,b) = ((x*a),(x*b))

andaCarro :: Bloco -> Float -> Terreno -> EstadoCarro -> EstadoCarro
andaCarro b t r c = c { posicao = p', velocidade = v' }
  where p' = (posicao c) .+. (fst (velocidade c)*t,snd (velocidade c)*t)
        v' = (velocidade c) .+. (t .*. ((accelVec r c) .+. (dragVec r c) .+. (driftVec r c) .+. (gravityVec r b c)))
       
accelVec :: Terreno -> EstadoCarro -> (Float,Float)
accelVec t c | acelera c && not (trava c) = arrowToComponents (delta_acel t,direcao c)
             | trava c && not (acelera c) = arrowToComponents (delta_acel t,direcao c + 180)
             | otherwise = (0,0)

dragVec :: Terreno -> EstadoCarro -> (Float,Float)
dragVec t c = arrowToComponents (delta_drag t*v,a + 180) 
  where (v,a) = componentsToArrow (velocidade c)

driftVec :: Terreno -> EstadoCarro -> (Float,Float)
driftVec t c = arrowToComponents (driftCoef,driftAngle)
  where (vv,av) = componentsToArrow $ velocidade c
        ad = direcao c
        driftAngle = if (sin (radians (av-ad))) > 0
                     then ad-90 -- going right 
                     else ad+90 -- going left
        driftCoef = vv * delta_drift t * abs (sin (radians (av-ad)))
    
gravityVec :: Terreno -> Bloco -> EstadoCarro -> (Float,Float)
gravityVec t (AltoBaixoVertical _ _) c = arrowToComponents (delta_gravity t, 90)
gravityVec t (BaixoAltoVertical _ _) c = arrowToComponents (delta_gravity t, 90)
gravityVec t (AltoBaixoHorizontal _ _) c = arrowToComponents (delta_gravity t, 0)
gravityVec t (BaixoAltoHorizontal _ _) c = arrowToComponents (delta_gravity t, 180)
gravityVec t _ c = (0,0)

-- geometry

radians th = th * (pi/180)
degrees th = th * (180/pi)

arrowToComponents :: Vector -> Point
arrowToComponents (v,th) = (getX v th,getY v th)
    where getX v th = v * cos (radians (th))
          getY v th = v * sin (radians (-th))

componentsToArrow :: Point -> Vector
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

-- gloss

glossBloco :: Float -> Bloco -> Picture
glossBloco tam b = case b of
    (PlanoVertical p i)        -> Color (corAltura p) $ Polygon [(-sd,sd),(-sd,-sd),(sd,-sd),(sd,sd)]
    (PlanoHorizontal p i)      -> Color (corAltura p) $ Polygon [(-sd,sd),(-sd,-sd),(sd,-sd),(sd,sd)]
    (AltoBaixoVertical p i)    -> transitaBloco tam (corAltura (p+1),corAltura p) False
    (AltoBaixoHorizontal p i)  -> transitaBloco tam (corAltura (p+1),corAltura p) True
    (BaixoAltoVertical p i)    -> transitaBloco tam (corAltura p,corAltura (p+1)) False
    (BaixoAltoHorizontal p i)  -> transitaBloco tam (corAltura p,corAltura (p+1)) True
    (CurvaDireitaBaixo p i)    -> Color (corAltura p) $ Polygon [(-sd,-sd),(sd,-sd),(sd,sd)]
    (CurvaEsquerdaBaixo p i)   -> Color (corAltura p) $ Polygon [(-sd,sd),(-sd,-sd),(sd,-sd)]
    (CurvaDireitaCima p i)     -> Color (corAltura p) $ Polygon [(-sd,sd),(sd,-sd),(sd,sd)]
    (CurvaEsquerdaCima p i)    -> Color (corAltura p) $ Polygon [(-sd,sd),(-sd,-sd),(sd,sd)]
    Lava -> Blank
  where sd = tam / 2

transitaBloco :: Float -> (Color,Color) -> Bool -> Picture
transitaBloco tam (c1,c2) i = Rotate g $ Pictures [a,b,c]
  where a = Color c1 $ Polygon [(-sd,sd),(sd,0),(-sd,-sd)]
        b = Color c2 $ Polygon [(-sd,sd),(sd,sd),(sd,0)]
        c = Color c2 $ Polygon [(-sd,-sd),(sd,-sd),(sd,0)]
        g = if i then 0 else 90
        sd = tam / 2

corAltura :: Altura -> Color
corAltura a = makeColor (0.2*toEnum a) (0.2*toEnum a) (0.15*toEnum a) 1

glossMapa :: Float -> (Float,Float) -> Mapa -> [Picture]
glossMapa tam (x,y) [] = []
glossMapa tam (x,y) ([]:ls) = glossMapa tam (0,y-tam) ls
glossMapa tam (x,y) ((c:cs):ls) = (Translate x y $ glossBloco tam c) : meta : glossMapa tam (x+tam,y) (cs:ls)
    where meta = if (c == PlanoHorizontal altInit 0) then Color green $ Translate x y (Line [(-sd,-sd),(-sd,sd)]) else Blank
          sd = tam / 2

glossCarro :: EstadoJogo -> Int -> Picture
glossCarro s i = Translate (x*tamBloco) (-y*tamBloco) $ Rotate (-a) pic
    where (x,y) = posicao (jogadores s!!i)
          a = direcao (jogadores s!!i)
          pic = Polygon [(-6,5),(6,0),(-6,-5)]
          tamBloco = realToFrac $ tamanhoBloco s

glossEvento :: Event -> EstadoJogo -> EstadoJogo
glossEvento e s = s { jogadores = c':tail (jogadores s) }
  where c' = glossEventoCarro e (jogadores s!!0)

glossEventoCarro :: Event -> EstadoCarro -> EstadoCarro
glossEventoCarro (EventKey (SpecialKey KeyDown ) kst _) e = e { trava   = kst == Down }
glossEventoCarro (EventKey (SpecialKey KeyUp   ) kst _) e = e { acelera = kst == Down }
glossEventoCarro (EventKey (SpecialKey KeyLeft ) kst _) e = e { viraE   = kst == Down }
glossEventoCarro (EventKey (SpecialKey KeyRight) kst _) e = e { viraD   = kst == Down }
glossEventoCarro _ st = st

glossDesenha :: EstadoJogo -> Picture
glossDesenha e = Translate (-(toEnum dimensao-1)*tamBloco/2) ((toEnum dimensao-1)*tamBloco/2) $ Pictures (m++p)
    where
    m = glossMapa tamBloco (0,0) (mapa e)
    p = map (glossCarro e) [0..3]
    tamBloco = realToFrac $ tamanhoBloco e

glossTempo :: Float -> EstadoJogo -> EstadoJogo
glossTempo t m = move t 0 m

--tamBloco :: Float
--tamBloco = 60
sd :: EstadoJogo -> Float
sd e = realToFrac (tamanhoBloco e) /2

joga :: IO ()
joga = do
    screen@(Display cx cy) <- getDisplay
    content <- CW.getTextContent
    caminho <- case readMaybe content :: Maybe Caminho of
        Just caminho -> return caminho
        Nothing -> error $ "failed to parse caminho " ++ show content
    let tamanhoX::Float = (realToFrac cx) / (realToFrac dimensao)
    let tamanhoY::Float = (realToFrac cy) / (realToFrac dimensao)
    let tamanho = min tamanhoX tamanhoY
    let back = red
    let ini = estadoInicial screen (round tamanho) (escava ex1)
    display screen back (glossDesenha ini)
    --play screen back ini glossDesenha glossEvento glossTempo
    where
--    screen = Display ((dimensao+1)*round tamBloco) ((dimensao+1)*round tamBloco)
--    screen = (InWindow "Novo Jogo" ((dimensao+1)*round tamBloco,(dimensao+1)*round tamBloco) (0, 0))
    
    
main = joga

-- exemplos
-- bom
ex1 :: Caminho
ex1 = [Segue,Esquerda,Segue,Direita,Segue,Direita,Sobe,Segue,Esquerda,Direita
      ,Esquerda,Direita,Direita,Esquerda,Direita,Esquerda,Esquerda,Segue,Segue
      ,Sobe,Direita,Direita,Segue,Segue,Sobe,Esquerda,Direita,Desce,Direita
      ,Esquerda,Direita,Esquerda,Segue,Direita,Desce,Desce,Segue,Segue,Direita,Segue]

-- out of bounds
exOB :: Caminho
exOB = [Segue,Segue,Direita,Segue,Segue,Esquerda,Segue,Direita,Direita
       ,Segue,Segue,Segue,Direita,Esquerda,Segue,Segue,Direita,Segue,Segue,Direita,Segue]      

-- overlaps
exOL :: Caminho
exOL = [Segue,Segue,Direita,Segue,Segue,Esquerda,Segue,Direita,Direita
       ,Segue,Direita,Segue,Direita,Esquerda,Segue,Direita,Segue,Segue,Direita,Segue]              

-- open
exOP :: Caminho
exOP = [Segue,Segue,Direita,Segue,Segue,Esquerda,Segue,Direita,Direita
       ,Segue,Segue,Segue,Direita,Esquerda,Segue,Direita,Segue,Segue,Direita]

-- height mismatch
exHM :: Caminho
exHM = [Segue,Segue,Direita,Segue,Segue,Esquerda,Segue,Direita,Direita
       ,Segue,Sobe,Segue,Direita,Esquerda,Segue,Direita,Segue,Segue,Direita]

