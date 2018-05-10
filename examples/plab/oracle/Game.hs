{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import LI11718
import OracleT4
import Mapas
import OracleT3
import System.Timeout
import OracleT2
import OracleT1
import System.Environment
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Test.QuickCheck.Gen
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Safe
import Data.Char
import Debug.Trace
import GHC.Float
import Text.Printf
import Data.Map (Map(..))
import qualified Data.Map as Map
--import qualified Tarefa6_2017li1g183 as G183
import qualified Tarefa6_2017li1g12 as G12
import qualified Tarefa6_2017li1g102 as G102
import qualified Tarefa6_2017li1g71 as G71


import Graphics.Gloss
import Graphics.Gloss.Data.Picture          
import Graphics.Gloss.Interface.Pure.Game   
import Graphics.Gloss.Interface.IO.Game   
import Graphics.Gloss.Juicy
import Graphics.Gloss.Geometry.Line

--import qualified BotHugo as Hugo
--import qualified BotNuno as Nuno

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
  , imagens :: String -> Float -> Picture
  , mapaNum :: Int
  , nomes :: [String]
  }

nextEstado :: Estado -> IO Estado 
nextEstado old = do
    let i = mapaNum old
    let j = mod (succ i) (length all_mapas -1)
    let mapa = atNote "nextEstado" all_mapas j
    prop <- generate $ elements propriedades
    let tam = getTamanho mapa
    return $ estadoInicial prop mapa j tam (imagens old) (nomes old)

estadoInicial :: Propriedades -> Mapa -> Int -> Float -> (String -> Float -> Picture) -> [String] -> Estado
estadoInicial x m num tam imgFun names = Estado { jogo = jogoInicial x m
                                    , acoes = replicate njogadores parado
                                    , timer = 0
                                    , tamBloco = tam
                                    , batotas = replicate njogadores 0
                                    , mortes = replicate njogadores 0
                                    , voltas = replicate njogadores (0,0)
                                    , imagens = imgFun
                                    , mapaNum = num, nomes = names }

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
    where  (Peca tp _) = (atNote2 "carroInicial" t b a)


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
                     | n>=0 = atualiza t (geraJogoT4 (n-1)) n (atNote "atualiza" a n)
        Mapa p0 tab = mapa jogoT4
        movimentaT3 i | c == Nothing = (l 0, 1.5, 0)
                      | batota iam wil = trace (show iam ++ " -> "++ show wil) (l 1, 0, 1.5)
                      | otherwise = (fromJustNote "atualiza" c, 0, 0)  
          where c = colide tab t (atNote "atualiza" (carros jogoT4) i)
                l k = lixa (mapa jogoT4) (head $ drop k (atNote "atualiza" (historico jogoT4) i)) (atNote "atualiza" (carros jogoT4) i)
                iam = snd $ atNote "atualiza" v i
                (wil,_) = whereAmI jogoT4 0 i

batota :: Int -> [Int] -> Bool
batota p0 p1 = minimum (map (\j -> j-p0) (sort p1)) > 4
              
lixa :: Mapa -> Posicao -> Carro -> Carro
lixa (Mapa _ t) (i,j) c = c { posicao = p , velocidade = (0,0) }
  where Peca tp _ = atNote2 "lixa" t j i 
        p = centroPeca tp (i,j)

--- begin gloss ---

getImage :: String -> Estado -> Picture
getImage name e = (imagens e) name (tamBloco e)

glossDesenha :: Estado -> IO Picture
glossDesenha e = return $ Pictures 
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
     where (x,y) = posicao (atNote "glossCarro" (carros $ jogo s) i)
           a = direcao (atNote "glossCarros" (carros $jogo s) i)
           pic = getImage ("c"++(show (i+1))) s
           nitpic i = getImage ("nitro" ++ show (i+1)) s
           pufpic = getImage "puff" s
           Mapa _ m = mapa (jogo s)
           t = atNote2 "glossCarro" m (floor y) (floor x) 
           isnit = isTarget s i
           nits = Pictures $ map nit isnit
           nit i | atNote "glossCarro" (nitros $ jogo s) i > 0 = Translate (-30) 0 (nitpic i)
                 | atNote "glossCarro" (nitros $ jogo s) i <= 0 = Translate (-30) 0 pufpic
                 | otherwise = Blank
 
isTarget :: Estado -> Int -> [Int]
isTarget e i = map fst (filter (\(x,y) -> y == Just i) as)
    where as = zip [0..] (map nitro $ acoes e)

glossEvento :: Event -> Estado -> IO Estado
glossEvento (EventKey (Char 'n') Down _ _) s = nextEstado s
glossEvento e s = do
    acoes' <- liftM Map.elems $ glossEventoCarro e (Map.fromAscList $ zip [0..] $ acoes s) 
    return s { acoes = acoes'}

glossEventoCarro :: Event -> Map Int Acao -> IO (Map Int Acao)
glossEventoCarro (EventKey (SpecialKey KeyDown ) kst _ _) e = return $ Map.adjust (\a -> a { travar   = kst == Down }) 0 e
glossEventoCarro (EventKey (SpecialKey KeyUp   ) kst _ _) e = return $ Map.adjust (\a -> a { acelerar = kst == Down }) 0 e 
glossEventoCarro (EventKey (SpecialKey KeyLeft ) kst _ _) e = return $ Map.adjust (\a -> a { esquerda = kst == Down }) 0 e 
glossEventoCarro (EventKey (SpecialKey KeyRight) kst _ _) e = return $ Map.adjust (\a -> a { direita  = kst == Down }) 0 e
glossEventoCarro (EventKey (Char n) Down _ _) e
    | isDigit n && (digitToInt n) `elem` [1..4] = return $ Map.adjust (\a -> a { nitro  = Just ((digitToInt n)-1) }) 0 e
--    | isDigit n && (digitToInt n) `elem` [5..8] = return $ Map.adjust (\a -> a { nitro  = Just ((digitToInt n)-5) }) 1 e 
    | otherwise = return e
glossEventoCarro (EventKey (Char n) Up _ _) e   
    | isDigit n && (digitToInt n) `elem` [1..4] = return $ Map.adjust (\a -> a { nitro  = Nothing }) 0 e
--    | isDigit n && (digitToInt n) `elem` [5..8] = return $ Map.adjust (\a -> a { nitro  = Nothing }) 1 e
    | otherwise = return e
glossEventoCarro _ st = return st

glossTempo :: Float -> Estado -> IO Estado
glossTempo t m = do
    let Mapa p0 tab = mapa (jogo m)
        prc = percorre [] tab (fst p0) (snd p0)
        botid1 = 1
        botid2 = 2
        botid3 = 3
    bot1 <- runBot G12.bot  (float2Double t) (jogo m) botid1 
    bot2 <- runBot G102.bot (float2Double t) (jogo m) botid2
    bot3 <- runBot G71.bot  (float2Double t) (jogo m) botid3
    let as1 = take botid1 (acoes m)++(bot1:drop (botid1+1) (acoes m))
    let as2 = take botid2 as1++(bot2:drop (botid2+1) as1)
    let as3 = take botid3 as2++(bot3:drop (botid3+1) as2)
    let as' = zipWith (\t a -> if t > 0 then parado else a)  (zipWith (+) (batotas m) (mortes m)) as3
    let (j',mrts,btts) = atualizaEMovimenta (float2Double t) (jogo m) (voltas m) as'
    let btts' = (zipWith (\a b -> (max a b)-t) (batotas m) btts)
    let mrts' = (zipWith (\a b -> (max a b)-t) (mortes m) mrts)
    let whereAreWe = map (whereAmI j' 0) [0..njogadores-1]
    let voltasI n
            | mxp-1 == act = (1 + fst (atNote "glossTempo" (voltas m) n),0)
            | otherwise = (fst (atNote "glossTempo" (voltas m) n),nxt)
          where
              (nxts,mxp) = atNote "glossTempo" whereAreWe n
              act = snd (atNote "glossTempo" (voltas m) n)
              nxt
                  | act `elem` nxts = act
                  | otherwise = maximumNote "glossTempo" $ filter (\k -> k <= 4 + act) nxts
    return $ m { jogo = j', timer = t + timer m
                   , batotas = btts'
                   , mortes = mrts'
                   , voltas = map voltasI [0..njogadores-1]
                   , acoes = as' }

whereAmI :: Jogo -> Int -> Int -> ([Int],Int)
whereAmI j p n = (r,length prc)
  where r = sortOn (\v -> abs $ p-v) f
        f = findIndices (\(_,i,_) -> i == head (atNote "whereI" (historico j) n)) prc
        Mapa p0 tab = mapa j
        prc = percorre [] tab (fst p0) (snd p0)
        

barraEstado :: Estado -> Picture
barraEstado e = Translate 10 (-10) $ Pictures $ timep:time:map f (zip [0..] (nitros (jogo e))) ++ mapInfo
    where f (i,t) = Translate 0 ((-60)-(fromIntegral i)*53) (Pictures (name:helm:mrt:btt:rnk:nbar))
            where helm = Translate 2 0 $ getImage ("p"++(show (i+1))) e 
                  name = Translate 90 (5    ) $ Color white $ Scale 0.15 0.15 $ Text $ atNote "nome" (nomes e) i
                  rnk = Translate 59 0 $ getImage (show (atNote "barra" wrs i)) e
                  nbar = [Translate (14*fromIntegral i) (-10) bar | i <- [1..ceiling t]]
                  btt = Translate 21 0 $ if (atNote "barra" (batotas e) i > 0) then getImage "btt" e else Blank
                  mrt = Translate 38 0 $ if (atNote "barra" (mortes e) i > 0) then getImage "mrt" e else Blank
                  bar = getImage ("bar"++show (i+1)) e
          time = Color white $ Translate 27 (-8) $ Scale 0.15 0.15 $ Text (printf "%05.2f" (timer e))
          timep = Translate 13 0 (Scale 0.05 0.05 $ getImage "timer" e)
          wrs = map snd $ sortOn (fst.fst) $ zip (reverse $ sortOn snd (zip [0..njogadores-1] (voltas e))) [1..njogadores]
          mapInfo =
              [Translate 0 (-270) $ Color white $ Scale 0.1 0.1 $ Text $ "Mapa " ++ show (succ $ mapaNum e) ++ " / " ++ show (length $ all_mapas)
              ,Translate 0 (-295) $ Color white $ Scale 0.1 0.1 $ Text "Keys:"
              ,Translate 0 (-320) $ Color white $ Scale 0.1 0.1 $ Text "Movement (arrow keys)"
              ,Translate 0 (-345) $ Color white $ Scale 0.1 0.1 $ Text "Nitro (1-4 number keys)"
              ,Translate 0 (-370) $ Color white $ Scale 0.1 0.1 $ Text "Next map (N)"
              ]

-- end gloss --

screenx = 1024
screeny = 768

getTamanho :: Mapa -> Float
getTamanho (Mapa _ tb) = realToFrac tamanho
    where
    x = toEnum (length (head tb))
    y = toEnum (length tb)
    tamanhoX::Float = (realToFrac screenx) / (realToFrac x)
    tamanhoY::Float = (realToFrac screeny) / (realToFrac y)
    tamanho = min tamanhoX tamanhoY

joga :: IO ()
joga = do    
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
        i <- generate $ chooseAny
        prop <- generate $ elements propriedades
        let j = mod i (length all_mapas -1)
            m@(Mapa (p,_) tb) = atNote "mapas" all_mapas j
            screen = (InWindow "MicroMachines" (screenx+200,screeny) (0, 0))
            back = greyN 0.5 
            tamanho = getTamanho m
            s = tamanho / 100
            imgFun :: String -> Float -> Picture
            imgFun "lava" tam = Translate (tam / 2) (-tam / 2) $ Scale (tam/100) (tam/100) lava 
            imgFun "nitro1" tam = nitro1
            imgFun "nitro2" tam = nitro2
            imgFun "nitro3" tam = nitro3
            imgFun "nitro4" tam = nitro4
            imgFun "puff" tam = puff
            imgFun "bar1" tam = Scale (2.5*(0.8)) (2*(0.8)) bar1
            imgFun "bar2" tam = Scale (2.5*(0.8)) (2*(0.8)) bar2
            imgFun "bar3" tam = Scale (2.5*(0.8)) (2*(0.8)) bar3
            imgFun "bar4" tam = Scale (2.5*(0.8)) (2*(0.8)) bar4
            imgFun "timer" tam = Translate 14 14 $ timer
            imgFun "1" tam = Translate 10 15 n1
            imgFun "2" tam = Translate 10 15 n2
            imgFun "3" tam = Translate 10 15 n3
            imgFun "4" tam = Translate 10 15 n4
            imgFun "btt" tam = Translate 14 14 $ Scale (2*(0.8)) (2*(0.8)) btt
            imgFun "mrt" tam = Translate 14 14 $ Scale (2*(0.8)) (2*(0.8)) mrt
            imgFun "p1" tam = Translate 14 14 $ Scale (2*(0.8)) (2*(0.8)) p1
            imgFun "p2" tam = Translate 14 14 $ Scale (2*(0.8)) (2*(0.8)) p2
            imgFun "p3" tam = Translate 14 14 $ Scale (2*(0.8)) (2*(0.8)) p3
            imgFun "p4" tam = Translate 14 14 $ Scale (2*(0.8)) (2*(0.8)) p4
            imgFun "c1" tam = Rotate 90 $ Scale (4*(0.8)) (4*(0.8)) c1
            imgFun "c2" tam = Rotate 90 $ Scale (4*(0.8)) (4*(0.8)) c2
            imgFun "c3" tam = Rotate 90 $ Scale (4*(0.8)) (4*(0.8)) c3
            imgFun "c4" tam = Rotate 90 $ Scale (4*(0.8)) (4*(0.8)) c4
            e = estadoInicial prop m j tamanho imgFun ["P1","G12","G102","G71"]
        playIO screen back 20 e glossDesenha glossEvento glossTempo

main = joga 


dir :: Ponto -> (Peca,Posicao,Orientacao) -> Double
dir p0 (Peca t _,p,_) = snd $ componentsToArrow (p'.-.p0)
  where p' = centroPeca t p

distRad :: Int -> Int -> Int
distRad r1 r2 = ((r2-r1) + 180) `mod` 360 - 180


type Bot = (Double -> Jogo -> Int -> Acao)

runBot :: Bot -> Double -> Jogo -> Int -> IO Acao
runBot bot tick jog p = {-trace ("running bot for " ++ show p) $-} catch (go) (\(err::SomeException) -> return parado)
  where
    go = do
        mb <- asyncTimeout_ (1 * 10 ^6) $ timeout (1 * 10 ^6) $! evaluate $! bot tick jog p
        case (join mb) of
            Nothing -> return $! (parado)
            Just j -> return $! (j)

asyncTimeout_ :: Int -> IO a -> IO (Maybe a)
asyncTimeout_ i f =
  withAsync f $ \a1 ->
  withAsync (threadDelay i) $ \a2 ->
  liftM (either Just (const Nothing)) $ race (wait a1) (wait a2)
