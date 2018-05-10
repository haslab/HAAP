{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AnimateT6 where

import JSImages
import LI11718
import OracleT1
import OracleT4
import OracleT3
import OracleT2
import System.Environment
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Safe
import Data.Char as Char
--import Debug.Trace
import GHC.Float
import Text.Printf
import Data.Text (Text(..))
import qualified Data.Text as Text
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Exception

import qualified CodeWorld as CW
import Graphics.Gloss hiding ((.*.),(.+.),(.-.))

import SimulateT6

data EstadoGloss = EstadoGloss
    { estadoScreen :: (Int,Int)
    , tamBloco :: Float
    , gMapa :: Mapa
    , gPista :: Propriedades
    , gFrame :: Frame
    , gTimer :: Float
    , images :: [(String,Picture)]
    , gPlayers :: [String]
    }

truncPlayer :: String -> String
truncPlayer "random" = "random"
truncPlayer xs = drop 8 xs

--- begin gloss ---

getImage x e = fromJustNote x $ lookup x (images e)

desenhaTexto :: EstadoGloss -> String -> Picture
desenhaTexto e s = Translate 30 0 $ Pictures picts
    where
    picts = desenhaTexto' s 0
    desenhaTexto' [] off = []
    desenhaTexto' (c:cs) off = (Translate (off) 0 scaledp:ps)
        where
        p@(Image w h img) = getImage ("f"++[Char.toLower c]) e
        scaledp = Scale (0.15) (0.15) p
        ps = desenhaTexto' cs (off+realToFrac w*0.15+2)


glossDesenha :: EstadoGloss -> Picture
glossDesenha e = Pictures 
                [Translate (-toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) (Pictures (m'++meta:p))
                ,Translate 0 (-20) barra] --,Translate ((-50)+toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) barra]
     where (Mapa ((i,j),dir_ori) m) = gMapa e
           m' = glossMapa e (0,0) m
           barra = barraEstado e
           p = map (glossCarro e) [0..njogadores - 1] -- [0..3]
           meta_ori = metaOri (atNote2 "meta" m j i) dir_ori
           meta = Color green $ Line $ metaLine meta_ori (realToFrac i) (realToFrac j) (tamBloco e)
           x = (length (head m))
           y = (length m)


glossBloco :: EstadoGloss -> Peca -> Picture 
glossBloco e (Peca Recta p)          = Color (corAltura (gPista e) p) $ Polygon [(0,-tamBloco e),(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]
glossBloco e (Peca (Rampa Norte) p)  = transitaBloco e (corAltura (gPista e) (p+1),corAltura (gPista e) p) False
glossBloco e (Peca (Rampa Oeste) p)  = transitaBloco e (corAltura (gPista e) (p+1),corAltura (gPista e) p) True
glossBloco e (Peca (Rampa Sul) p)    = transitaBloco e (corAltura (gPista e) p,corAltura (gPista e) (p+1)) False
glossBloco e (Peca (Rampa Este) p)   = transitaBloco e (corAltura (gPista e) p,corAltura (gPista e) (p+1)) True
glossBloco e (Peca (Curva Oeste) p)  = Pictures [getImage "lava" e,Color (corAltura (gPista e) p) $ Polygon [(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]]
glossBloco e (Peca (Curva Sul) p)    = Pictures [getImage "lava" e,Color (corAltura (gPista e) p) $ Polygon [(0,-tamBloco e),(tamBloco e,0),(0,0)]]
glossBloco e (Peca (Curva Norte) p)  = Pictures [getImage "lava" e,Color (corAltura (gPista e) p) $ Polygon [(0,-tamBloco e),(tamBloco e,-tamBloco e),(tamBloco e,0)]]
glossBloco e (Peca (Curva Este) p)   = Pictures [getImage "lava" e,Color (corAltura (gPista e)p) $ Polygon [(0,0),(0,-tamBloco e),(tamBloco e,-tamBloco e)]]
glossBloco e (Peca Lava _)  = getImage "lava" e

numalturas = 5
corAltura :: Propriedades -> Altura -> Color
corAltura p a | a >= 0 = makeColor (r*tr) (r*tg) (r*tb) 1
    where r = (toEnum a) * 1 / numalturas
          tr = 1+0.2*(double2Float $ k_atrito p)*0.5
          tg = 1+0.2*(3 - (double2Float $ k_atrito p))*0.5
          tb = 1+0.2*(2 - (double2Float $ k_atrito p))
corAltura p a | a < 0  = makeColor (r*tr) (r*tg) (r*tb) 1
    where r = abs (toEnum a) * 1 / numalturas
          tr = (double2Float $ k_atrito p)*0.5
          tg = (3 - (double2Float $ k_atrito p))*0.5
          tb = (2 - (double2Float $ k_atrito p))

transitaBloco :: EstadoGloss -> (Color,Color) -> Bool -> Picture
transitaBloco e (c1,c2) i = Translate 0 gy $ Rotate g $ Pictures [a,b,c]
  where a = Color c1 $ Polygon [(0,0),(tamBloco e/2,-tamBloco e),(tamBloco e,0)]
        b = Color c2 $ Polygon [(0,0),(tamBloco e/2,-tamBloco e),(0,-tamBloco e)]
        c = Color c2 $ Polygon [(tamBloco e,0),(tamBloco e/2,-tamBloco e),(tamBloco e,-tamBloco e)]
        g = if i then -90 else 0
        gy = if i then -tamBloco e else 0

glossMapa :: EstadoGloss -> (Float,Float) -> Tabuleiro -> [Picture]
glossMapa e (x,y) [] = []
glossMapa e (x,y) ([]:ls) = glossMapa e (0,y-tamBloco e) ls
glossMapa e (x,y) ((c:cs):ls) = (Translate x y $ glossBloco e c) : glossMapa e (x+tamBloco e,y) (cs:ls)

glossCarro :: EstadoGloss -> Int -> Picture
glossCarro s i = showerr err cpic
    where
    fri = atNote "frame" fr i
    err = frameError fri
    showerr Nothing = id
    showerr (Just msg) = CW.trace (Text.pack $ "player " ++ show i ++ " error: " ++ msg)
    fr = gFrame s
    cpic = Translate (double2Float x*tamBloco s) (-double2Float y*tamBloco s) $ Scale 0.5 0.5 $ Rotate (-double2Float a) (Pictures [nit,pic])
    (x,y) = framePosicao fri
    a = frameDirecao fri
    pic = getImage ("c"++(show (i+1))) s
    nitpic i = getImage ("nitro" ++ show (i+1)) s
    pufpic = getImage "puff" s
    Mapa _ m = gMapa s
    t = atNote "floor" (atNote "floor" m (floor y)) (floor x) 
    nit = case usedNitro fri of
        Nitro is -> Pictures $ map (Translate (-tamBloco s / 3) 0 . nitpic) is
        Puff -> Translate (-tamBloco s / 3) 0 pufpic
        NoFuel -> Blank

barraEstado :: EstadoGloss -> Picture
barraEstado e = Pictures $ map drawPlayerState (zip [0..] $ map frameNitro $ gFrame e)
    where
    (screenx,screeny) = estadoScreen e
    pscreenx = realToFrac screenx / 4
    drawPlayerState (i,t) = Translate (20 - realToFrac screenx / 2 + pscreenx * realToFrac i) (realToFrac screeny / 2 - 15) $ Pictures [name,helm,rnk,btt,mrt,nbar]
        where
        fr = atNote "gframe" (gFrame e) i
        name = Translate 30 0 $ desenhaTexto e (truncPlayer $ atNote "gplayers" (gPlayers e) i)
        helm = getImage ("p"++(show (i+1))) e 
        rnk = Translate 25 0 $ getImage (show $ frameRank fr) e
        btt = Translate 0 (-10) $ if (frameBatota fr > 0) then getImage "btt" e else Blank
        mrt = Translate 0 (-10) $ if (frameTimeout fr || frameMorte fr > 0) then getImage "mrt" e else Blank
        bar = getImage ("bar"++show (i+1)) e
        nbar = Translate (-7) 0 $ Pictures [Translate (8*fromIntegral i) (-25) bar | i <- [0..ceiling t]]
        

--barraEstado :: EstadoGloss -> Picture
--barraEstado e = Translate (-30 + tamBloco e / 2) (-tamBloco e / 2) $ Pictures $ {-timep:time:-}map f (zip [0..] (map frameNitro $ gFrame e))
--    where
--    f (i,t) = Translate 0 ((-60)-(fromIntegral i)*53) (Pictures (helm:mrt:btt:rnk:name:nbar))
--        where
--        nbar = [Translate (14*fromIntegral i) (-10) bar | i <- [1..ceiling t]]
--        bar = getImage ("bar"++show (i+1)) e
--    --time = Color white $ Translate 27 (-8) $ Scale 0.15 0.15 $ Text (printf "%05.2f" (gTimer e))
--    --timep = Translate 13 0 (Scale 0.05 0.05 $ getImage "timer" e)
--    Mapa p0 m = gMapa e

-- end gloss --

joga :: Mapa -> Propriedades -> Frames -> [String] -> IO ()
joga mapa@(Mapa _ tab) pista frames players = do   
        let l::Int = length frames
        let f::Frame = maybe [] id $ lastMay frames
        screen@(Display screenx screeny) <- getDisplay 
        let x::Int = length (head tab)
            y::Int = length tab
            tamanhoX::Float = (realToFrac $ screenx) / (realToFrac x)
            tamanhoY::Float = (realToFrac $ screeny - 150) / (realToFrac y)
            tamanho = min tamanhoX tamanhoY
            back = greyN 0.5   
        let getFrame :: Float -> Frame
            getFrame t = if i >= l then f else atNote "frames" frames i
                where i = round (t * 20)
        imgs <- loadImages tamanho screen
        let drawFrames :: Float -> Picture
            drawFrames t = glossDesenha (EstadoGloss (screenx,screeny) tamanho mapa pista (getFrame t) 0 imgs players)
        animate screen back drawFrames

animaT6 :: Mapa -> Propriedades -> Frames -> [String] -> IO ()
animaT6 mapa pista frames players = catch (joga mapa pista frames players) $ \(e::SomeException) -> CW.trace (Text.pack $ displayException e) $ throw e

dir :: Ponto -> (Peca,Posicao,Orientacao) -> Double
dir p0 (Peca t _,p,_) = snd $ componentsToArrow (p'.-.p0)
  where p' = centroPeca t p

distRad :: Int -> Int -> Int
distRad r1 r2 = ((r2-r1) + 180) `mod` 360 - 180
