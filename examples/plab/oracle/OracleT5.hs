{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import LI11718
import OracleT4
import Mapas
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
import qualified Tarefa6_2017li1g183 as G183

import Graphics.Gloss
import Graphics.Gloss.Data.Picture          
import Graphics.Gloss.Interface.Pure.Game   
import Graphics.Gloss.Juicy
import Graphics.Gloss.Geometry.Line

import qualified BotHugo as Hugo
import qualified BotNuno as Nuno

--terra = Propriedades 2 3 4 2 15 180
--gelo = Propriedades 0.3 0.4 2 1.5 15 270
--asfalto = Propriedades 4 5 8 4 10 120


parado = Acao False False False False Nothing

data Estado = Estado 
  { jogo :: Jogo 
  , acoes :: [Acao]
  , timer :: Float
  , tamBloco :: Float
  , batotas :: [Float]
  , mortes :: [Float]
  , voltas :: [(Int,Int)]
  , imagens :: [(String,Picture)]
  }

estadoInicial :: Propriedades -> Mapa -> Float -> [(String,Picture)] -> Estado
estadoInicial x m tam imgs = Estado { jogo = jogoInicial x m
                                    , acoes = replicate njogadores parado
                                    , timer = 0
                                    , tamBloco = tam
                                    , batotas = replicate njogadores 0
                                    , mortes = replicate njogadores 0
                                    , voltas = replicate njogadores (0,0)
                                    , imagens = imgs }

njogadores = 4
qntnitro = 5

jogoInicial :: Propriedades -> Mapa -> Jogo
jogoInicial x m@(Mapa p t) = Jogo { mapa      = m 
                                  , pista     = x
                                  , carros    = map (carroInicial t (fst p)) [0..njogadores-1]
                                  , nitros    = replicate njogadores qntnitro
                                  , historico = replicate njogadores [(fst p)]
                                  }

carroInicial :: Tabuleiro -> Posicao -> Int -> Carro
carroInicial t (a,b) i = Carro { posicao    = centroPeca tp (a,b)
                               , direcao    = 0
                               , velocidade = (0,0)
                               }
    where  (Peca tp _) = (t!!b!!a)


atualizaEMovimenta :: Tempo -> Jogo -> [(Int,Int)] -> [Acao] -> (Jogo,[Float],[Float])
atualizaEMovimenta t j v a = (jogoT4 
                                { carros = map (\(i,_,_) -> i) carrosT3
                                , historico = zipWith fhis (historico jogoT4) carrosT3}
                           , map (\(_,i,_) -> i) carrosT3
                           , map (\(_,_,i) -> i) carrosT3)
  where fhis l (_,_,0) = l
        fhis l _       = tail l
        jogadores = length (carros j) - 1
        jogoT4    = geraJogoT4 jogadores
        carrosT3  = map movimentaT3 [0..jogadores]
        geraJogoT4 n | n<0  = j
                     | n>=0 = atualiza t (geraJogoT4 (n-1)) n (a!!n)
        Mapa p0 tab = mapa jogoT4
        movimentaT3 i | c == Nothing = (l 0, 1.5, 0)
                      | batota iam wil = trace (show iam ++ " -> "++ show wil) (l 1, 0, 1.5)
                      | otherwise = (fromJustNote "atualiza" c, 0, 0)  
          where c = colide tab t (carros jogoT4 !! i)
                l k = lixa (mapa jogoT4) (head $ drop k (historico jogoT4!!i)) (carros jogoT4!!i)
                iam = snd $ v!!i
                (wil,_) = whereAmI jogoT4 0 i

batota :: Int -> [Int] -> Bool
batota p0 p1 = minimum (map (\j -> j-p0) (sort p1)) > 4
              
lixa :: Mapa -> Posicao -> Carro -> Carro
lixa (Mapa _ t) (i,j) c = c { posicao = p , velocidade = (0,0) }
  where Peca tp _ = t!!j!!i 
        p = centroPeca tp (i,j)

--- begin gloss ---

getImage x e = fromJustNote "getimage" $ lookup x (imagens e)

glossDesenha :: Estado -> Picture
glossDesenha e = Pictures 
                [Translate ((-100)-toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) (Pictures (m'++meta:p))
                ,Translate ((-100)+toEnum x*(tamBloco e)/2) (toEnum y*(tamBloco e)/2) barra]
     where (Mapa ((i,j),_) m) = mapa (jogo e)
           m' = glossMapa e (0,0) m
           barra = barraEstado e
           p = map (glossCarro e) [0..njogadores - 1] -- [0..3]
           meta = Color green $ Line [((toEnum i*tamBloco e),-(toEnum j*tamBloco e))
                                     ,((toEnum i*tamBloco e),-(toEnum j*tamBloco e)-tamBloco e)]
           x = (length (head m))
           y = (length m)


glossBloco :: Estado -> Peca -> Picture 
glossBloco e (Peca Recta p)          = Color (corAltura (pista (jogo e)) p) $ Polygon [(0,-tamBloco e),(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]
glossBloco e (Peca (Rampa Norte) p)  = transitaBloco e (corAltura (pista (jogo e)) (p+1),corAltura (pista (jogo e)) p) False
glossBloco e (Peca (Rampa Oeste) p)  = transitaBloco e (corAltura (pista (jogo e)) (p+1),corAltura (pista (jogo e)) p) True
glossBloco e (Peca (Rampa Sul) p)    = transitaBloco e (corAltura (pista (jogo e)) p,corAltura (pista (jogo e)) (p+1)) False
glossBloco e (Peca (Rampa Este) p)   = transitaBloco e (corAltura (pista (jogo e)) p,corAltura (pista (jogo e)) (p+1)) True
glossBloco e (Peca (Curva Oeste) p)  = Pictures [getImage "lava" e,Color (corAltura (pista (jogo e)) p) $ Polygon [(0,0),(tamBloco e,0),(tamBloco e,-tamBloco e)]]
glossBloco e (Peca (Curva Sul) p)    = Pictures [getImage "lava" e,Color (corAltura (pista (jogo e)) p) $ Polygon [(0,-tamBloco e),(tamBloco e,0),(0,0)]]
glossBloco e (Peca (Curva Norte) p)  = Pictures [getImage "lava" e,Color (corAltura (pista (jogo e)) p) $ Polygon [(0,-tamBloco e),(tamBloco e,-tamBloco e),(tamBloco e,0)]]
glossBloco e (Peca (Curva Este) p)   = Pictures [getImage "lava" e,Color (corAltura (pista (jogo e)) p) $ Polygon [(0,0),(0,-tamBloco e),(tamBloco e,-tamBloco e)]]
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

glossCarro :: Estado -> Int -> Picture
glossCarro s i = Translate (double2Float x*tamBloco s) (-double2Float y*tamBloco s) $ Scale 0.5 0.5 $ Rotate (-double2Float a) (Pictures [nits,pic])
     where (x,y) = posicao (carros (jogo s)!!i)
           a = direcao (carros (jogo s)!!i)
           pic = getImage ("c"++(show (i+1))) s
           nitpic i = getImage ("nitro" ++ show (i+1)) s
           pufpic = getImage "puff" s
           Mapa _ m = mapa (jogo s)
           t = m!!(floor y)!!(floor x) 
           isnit = isTarget s i
           nits = Pictures $ map nit isnit
           nit i | (nitros (jogo s))!!i > 0 = Translate (-30) 0 (nitpic i)
                 | (nitros (jogo s))!!i <= 0 = Translate (-30) 0 pufpic
                 | otherwise = Blank
 
isTarget :: Estado -> Int -> [Int]
isTarget e i = map fst (filter (\(x,y) -> y == Just i) as)
    where as = zip [0..] (map nitro $ acoes e)

glossEvento :: Event -> Estado -> Estado
glossEvento e s = s { acoes = Map.elems $ glossEventoCarro e (Map.fromAscList $ zip [0..] $ acoes s) }

glossEventoCarro :: Event -> Map Int Acao -> Map Int Acao
glossEventoCarro (EventKey (SpecialKey KeyDown ) kst _ _) e = Map.adjust (\a -> a { travar   = kst == Down }) 0 e
glossEventoCarro (EventKey (SpecialKey KeyUp   ) kst _ _) e = Map.adjust (\a -> a { acelerar = kst == Down }) 0 e 
glossEventoCarro (EventKey (SpecialKey KeyLeft ) kst _ _) e = Map.adjust (\a -> a { esquerda = kst == Down }) 0 e 
glossEventoCarro (EventKey (SpecialKey KeyRight) kst _ _) e = Map.adjust (\a -> a { direita  = kst == Down }) 0 e
glossEventoCarro (EventKey (Char n) Down _ _) e
    | isDigit n && (digitToInt n) `elem` [1..4] = Map.adjust (\a -> a { nitro  = Just ((digitToInt n)-1) }) 0 e
    | isDigit n && (digitToInt n) `elem` [5..8] = Map.adjust (\a -> a { nitro  = Just ((digitToInt n)-5) }) 1 e 
    | otherwise = e
glossEventoCarro (EventKey (Char n) Up _ _) e   
    | isDigit n && (digitToInt n) `elem` [1..4] = Map.adjust (\a -> a { nitro  = Nothing }) 0 e
    | isDigit n && (digitToInt n) `elem` [5..8] = Map.adjust (\a -> a { nitro  = Nothing }) 1 e
    | otherwise = e
glossEventoCarro _ st = st

glossTempo :: Float -> Estado -> Estado
glossTempo t m = m { jogo = j', timer = t + timer m
                   , batotas = btts'
                   , mortes = mrts'
                   , voltas = map voltasI [0..njogadores-1]
                   , acoes = as' }
  where (j',mrts,btts) = atualizaEMovimenta (float2Double t) (jogo m) (voltas m) as'
        Mapa p0 tab = mapa (jogo m)
        prc = percorre [] tab (fst p0) (snd p0)
        whereAreWe = map (whereAmI j' 0) [0..njogadores-1]
        voltasI n | mxp-1 == act = (1 + fst ((voltas m)!!n),0)
                  | otherwise = (fst (voltas m!!n),nxt)
            where (nxts,mxp) = whereAreWe!!n
                  act = snd ((voltas m)!!n)
                  nxt | act `elem` nxts = act
                      | otherwise = maximumNote "glossTempo" $ filter (\k -> k <= 4 + act) nxts
        as1 = take botnunoid (acoes m)++(botnuno:drop (botnunoid+1) (acoes m))
        as2 = take bothugoid (acoes m)++(bothugo:drop (bothugoid+1) as1)
        botnuno = Nuno.bot (float2Double t) (jogo m) botnunoid
        botnunoid = 3
        bothugo = if odd (floor t)
            then Hugo.bot (float2Double t) (jogo m) bothugoid
            else decide (float2Double t) m bothugoid
        bothugoid = 2
        btts' = (zipWith (\a b -> (max a b)-t) (batotas m) btts)
        mrts' = (zipWith (\a b -> (max a b)-t) (mortes m) mrts)
        as' = zipWith (\t a -> if t > 0 then parado else a)  (zipWith (+) (batotas m) (mortes m)) as2

whereAmI :: Jogo -> Int -> Int -> ([Int],Int)
whereAmI j p n = (r,length prc)
  where r = sortOn (\v -> abs $ p-v) f
        f = findIndices (\(_,i,_) -> i == head ((historico j)!!n)) prc
        Mapa p0 tab = mapa j
        prc = percorre [] tab (fst p0) (snd p0)
        

barraEstado :: Estado -> Picture
barraEstado e = Translate 10 (-10) $ Pictures $ timep:time:map f (zip [0..] (nitros (jogo e)))
    where f (i,t) = Translate 0 ((-60)-(fromIntegral i)*53) (Pictures (helm:mrt:btt:rnk:nbar))
            where helm = Translate 2 0 $ getImage ("p"++(show (i+1))) e 
                  rnk = Translate 59 0 $ getImage (show (wrs!!i)) e
                  nbar = [Translate (14*fromIntegral i) (-10) bar | i <- [1..ceiling t]]
                  btt = Translate 21 0 $ if (batotas e!!i > 0) then getImage "btt" e else Blank
                  mrt = Translate 38 0 $ if (mortes e!!i > 0) then getImage "mrt" e else Blank
                  bar = getImage ("bar"++show (i+1)) e
          time = Color white $ Translate 27 (-8) $ Scale 0.15 0.15 $ Text (printf "%05.2f" (timer e))
          timep = Translate 13 0 (Scale 0.05 0.05 $ getImage "timer" e)
          wrs = map snd $ sortOn (fst.fst) $ zip (reverse $ sortOn snd (zip [0..njogadores-1] (voltas e))) [1..njogadores]

-- end gloss --

joga :: Int -> Int -> IO ()
joga i j = do    
        Just lava  <- loadJuicy "lava.jpg"
        Just nitro1 <- loadJuicy "nitro1.png"
        Just nitro2 <- loadJuicy "nitro2.png"
        Just nitro3 <- loadJuicy "nitro3.png"
        Just nitro4 <- loadJuicy "nitro4.png"
        Just puff <- loadJuicy "puff.png"
        Just bar1 <- loadJuicy "bar1.png"
        Just bar2 <- loadJuicy "bar2.png"
        Just bar3 <- loadJuicy "bar3.png"
        Just bar4 <- loadJuicy "bar4.png"
        Just p1 <- loadJuicy "p1.png"
        Just p2 <- loadJuicy "p2.png"
        Just p3 <- loadJuicy "p3.png"
        Just p4 <- loadJuicy "p4.png"
        Just c1 <- loadJuicy "c1.png"
        Just c2 <- loadJuicy "c2.png"
        Just c3 <- loadJuicy "c3.png"
        Just c4 <- loadJuicy "c4.png"
        Just n1 <- loadJuicy "1.png"
        Just n2 <- loadJuicy "2.png"
        Just n3 <- loadJuicy "3.png"
        Just n4 <- loadJuicy "4.png"
        Just btt <- loadJuicy "btt.png"
        Just mrt <- loadJuicy "mrt.png"
        Just timer <- loadJuicy "timer.png"
        let e = (estadoInicial prp ini tamanho imgs)
            imgs = [("lava",Translate (tamBloco e / 2) (-tamBloco e / 2) $ Scale s s lava)
                   ,("nitro1",nitro1),("nitro2",nitro2),("nitro3",nitro3),("nitro4",nitro4)
                   ,("puff",puff)
                   ,("bar1",Scale (2.5*s) (2*s) bar1)
                   ,("bar2",Scale (2.5*s) (2*s) bar2)
                   ,("bar3",Scale (2.5*s) (2*s) bar3)
                   ,("bar4",Scale (2.5*s) (2*s) bar4)
                   ,("timer",Translate 14 14 $ timer)
                   ,("1",Translate 10 15 n1)
                   ,("2",Translate 10 15 n2)
                   ,("3",Translate 10 15 n3)
                   ,("4",Translate 10 15 n4)
                   ,("btt",Translate 14 14 $ Scale (2*s) (2*s) btt)
                   ,("mrt",Translate 14 14 $ Scale (2*s) (2*s) mrt)
                   ,("p1",Translate 14 14 $ Scale (2*s) (2*s) p1)
                   ,("p2",Translate 14 14 $ Scale (2*s) (2*s) p2)
                   ,("p3",Translate 14 14 $ Scale (2*s) (2*s) p3)
                   ,("p4",Translate 14 14 $ Scale (2*s) (2*s) p4)
                   ,("c1",Rotate 90 $ Scale (4*s) (4*s) c1)
                   ,("c2",Rotate 90 $ Scale (4*s) (4*s) c2)
                   ,("c3",Rotate 90 $ Scale (4*s) (4*s) c3)
                   ,("c4",Rotate 90 $ Scale (4*s) (4*s) c4)]
            m@(Mapa (p,_) tb) = mapa (jogo e)
        play screen back 20 e glossDesenha glossEvento glossTempo
  where
    screenx = 1024
    screeny = 768
    screen = (InWindow "MicroMachines" (screenx+200,screeny) (0, 0))
    back = greyN 0.5 
    x = toEnum (length (head m))
    y = toEnum (length m)
    tamanhoX::Float = (realToFrac screenx) / (realToFrac x)
    tamanhoY::Float = (realToFrac screeny) / (realToFrac y)
    tamanho = min tamanhoX tamanhoY
    s = tamanho / 100
    ini@(Mapa p m) = mapas_torneio!!i --mapas5!!i --mapas4!!i {-mapas5!!i-} --mapas16!!i
    prp = pisos!!j

pisos = [terra,asfalto,gelo]
mapas5 = [Mapa ((3,1),Este) testeMapa, constroi [Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,CurvaDir,Desce,Avanca,CurvaEsq,CurvaDir
      ,CurvaEsq,CurvaDir,CurvaDir,CurvaEsq,CurvaDir,CurvaEsq,CurvaEsq,Avanca,Avanca
      ,Desce,CurvaDir,CurvaDir,Avanca,Avanca,Desce,CurvaEsq,CurvaDir,Sobe,CurvaDir
      ,CurvaEsq,CurvaDir,CurvaEsq,Avanca,CurvaDir,Sobe,Sobe,Avanca,Avanca,CurvaDir,Avanca], constroi [Avanca,CurvaDir,CurvaDir,CurvaDir,Avanca,CurvaDir,Avanca,CurvaDir
        ,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaDir
        ,Avanca,Avanca,Avanca,CurvaDir,Sobe,Avanca,Sobe,Avanca,CurvaDir,Avanca,Desce,Avanca,Desce
        ,CurvaDir,Desce,Sobe,CurvaDir],constroi [Sobe,CurvaDir,CurvaDir,Desce,CurvaDir,CurvaDir],Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                            ,[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                            ,[Peca Lava 0,Peca (Rampa Norte) (-1),Peca Lava 0,Peca (Curva Oeste) 1,Peca (Curva Este) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                            ,[Peca Lava 0,Peca (Curva Oeste) (-1),Peca Recta (-1),Peca (Curva Este) (-1),Peca (Rampa Sul) 1,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                            ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta (-1),Peca (Rampa Sul) 2,Peca Lava 0,Peca Lava 0,Peca Lava 0]
                            ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) (-1),Peca (Curva Oeste) 3,Peca (Rampa Oeste) 2,Peca (Curva Este) 2,Peca Lava 0]
                            ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 1,Peca Lava 0]
                            ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca (Curva Oeste) 1,Peca (Rampa Este) 1,Peca (Rampa Oeste) 1,Peca (Curva Sul) 1,Peca Lava 0]
                            ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]]

main = do args <- getArgs 
          joga (read (args!!0)) (read (args!!1))

-- Tarefa6 

decide :: Double -> Estado -> Int -> Acao
decide t e i | p' == Nothing = Acao False True (trg>0) (trg<0) Nothing
             | otherwise = Acao (v<1.5) False (trg>0) (trg<0) Nothing 
    where p = (carros (jogo e)!!i) 
          p' = colide m (4*t) (carros (jogo e)!!i) 
          Mapa ((pi,pj),pe) m = mapa (jogo e)
          Peca tp _ = (m!!pj!!pi)
          prc = percorre [] m (pi,pj) pe
          whereAmI = dropWhile (\(_,i,_) -> i /= ponto2Pos (posicao p)) (prc++prc)
          whereAmI' = dropWhile (\(_,i,_) -> i /= ponto2Pos (posicao p)) (tail whereAmI)
          dr0 = dir (posicao p) (whereAmI!!1)
          dr' = if length whereAmI' > 0 then dir (posicao p) (whereAmI'!!1) else dr0
          trg0 = distRad (round $ direcao p) (round dr0)
          trg1 = distRad (round $ direcao p) (round dr')
          trg = if abs trg0 < abs trg1 then trg0 else trg1
          (v,_) = componentsToArrow (velocidade p)
          ntc = (round (timer e) `mod`) (njogadores + 1)
          nit | ntc == 4 = Nothing
              | otherwise = Just ntc

dir :: Ponto -> (Peca,Posicao,Orientacao) -> Double
dir p0 (Peca t _,p,_) = snd $ componentsToArrow (p'.-.p0)
  where p' = centroPeca t p

distRad :: Int -> Int -> Int
distRad r1 r2 = ((r2-r1) + 180) `mod` 360 - 180
