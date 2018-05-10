{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}

module SimulateT6 where


import LI11718
import OracleT4
import OracleT3
import OracleT2
import System.Environment
import System.Timeout
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Safe
import Data.Char
import GHC.Float
import Text.Printf
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Binary
import GHC.Generics (Generic(..))
import Data.Typeable
import qualified Data.ByteString.Lazy as BS
import System.IO

--import Debug.Trace
import Mapas

--import Debug.Trace

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

isNone guiargs = isGUINone (guiP1 guiargs)
              && isGUINone (guiP2 guiargs)
              && isGUINone (guiP3 guiargs)
              && isGUINone (guiP4 guiargs)

isGUINone (GUIBot _) = False
isGUINone GUINone = True



data GUIPlayer
    = GUIBot Bot
    | GUINone

type Frames = [Frame]
    
type Frame = [CarroFrame]
  
data CarroFrame = CarroFrame
    { framePosicao :: Ponto
    , frameDirecao :: Angulo
    , frameNitro :: Tempo
    , frameRank :: Int
    , frameBatota :: Float
    , frameMorte :: Float
    , frameTimeout :: Bool
    , usedNitro :: Nitro
    , frameError :: Maybe String
    }
  deriving (Eq,Read,Show,Generic,Typeable)

instance Binary CarroFrame

data Nitro = Nitro [Int]
           | Puff
           | NoFuel
  deriving (Eq,Read,Show,Generic,Typeable)
instance Binary Nitro

data Estado = Estado
    { estadoJogo :: Jogo
    , estadoBatotas :: [Float]
    , estadoMortes :: [Float]
    , estadoNitros :: [Nitro]
    , estadoVoltas :: [(Int,Int)]
    , estadoTimeouts :: [Bool]
    }

estadoPosicoes :: Estado -> [Int]
estadoPosicoes e = map volta2posicao $ estadoVoltas e
    where
    size = length $ pecasMapa $ mapa $ estadoJogo e
    volta2posicao (v,p) = v * size + p

estadoFinal :: Estado -> Bool
estadoFinal x = sum (map fst $ estadoVoltas x) >= 1

estadoInicial :: Mapa -> Propriedades -> Estado
estadoInicial mapa propriedades = Estado (jogoInicial mapa propriedades) batotas mortes nitros voltas timeouts
    where
    batotas = replicate njogadores 0
    mortes = replicate njogadores 0
    nitros = replicate njogadores NoFuel
    voltas = replicate njogadores (0,0)
    timeouts = replicate njogadores False

estadoFrame :: Estado -> [Maybe String] -> Frame
estadoFrame e errs = map carroFrame (zip3 [0..] cs errs)
    where
    j = estadoJogo e
    cs = carros j
    carroFrame :: (Int,Carro,Maybe String) -> CarroFrame
    carroFrame (i,c,err) = CarroFrame (posicao c) (direcao c) (atNote "nitro" (nitros j) i) (atNote "wrs" wrs i) (atNote "batotas" (estadoBatotas e) i) (atNote "mortes" (estadoMortes e) i) (atNote "timeouts" (estadoTimeouts e) i) (atNote "nitros" (estadoNitros e) i) err
    -- atualizar rank
    wrs = map snd $ sortOn (fst.fst) $ zip (reverse $ sortOn snd (zip [0..njogadores-1] (estadoVoltas e))) [1..njogadores]

type Bot = (Double -> Jogo -> Int -> Acao)

runBot :: Bot -> Double -> Jogo -> Int -> IO (Acao,Maybe String,Bool)
runBot bot tick jog p = {-trace ("running bot for " ++ show p) $-} catch (go) (\(err::SomeException) -> return (parado,Just $ displayException err,False))
    where
    go = do
        mb <- asyncTimeout_ (1 * 10 ^6) $ timeout (1 * 10 ^6) $! evaluate $! force $! bot tick jog p
        case (join mb) of
            Nothing -> return $! (parado,Just "timeout",True)
            Just j -> return $! (j,Nothing,False)

asyncTimeout_ :: Int -> IO a -> IO (Maybe a)
asyncTimeout_ i f =
  withAsync f $ \a1 ->
  withAsync (threadDelay i) $ \a2 ->
  liftM (either Just (const Nothing)) $ race (wait a1) (wait a2)

guiBot GUINone = noBot
guiBot (GUIBot bot) = bot

noBot :: Bot
noBot _ _ _ = parado

data GUIArgs = GUIArgs
    { guiMap :: Mapa
    , guiPista :: Propriedades
    , guiP1 :: GUIPlayer
    , guiP2 :: GUIPlayer
    , guiP3 :: GUIPlayer
    , guiP4 :: GUIPlayer
    }

standard = Propriedades (1.5) 1 4 2 15 90

parado = Acao False False False False Nothing



resultado :: Frame -> [Int]
resultado = map frameRank

--centroPeca :: Tipo -> Posicao -> Ponto
--centroPeca (Curva Norte) (a,b) = (toEnum a+0.7,toEnum b+0.7)
--centroPeca (Curva Este) (a,b) = (toEnum a+0.3,toEnum b+0.7)
--centroPeca (Curva Sul) (a,b) = (toEnum a+0.3,toEnum b+0.3)
--centroPeca (Curva Oeste) (a,b) = (toEnum a+0.7,toEnum b+0.3)
--centroPeca _ (a,b) = (toEnum a+0.5,toEnum b+0.5)


njogadores = 4
qntnitro = 5

jogoInicial :: Mapa -> Propriedades -> Jogo
jogoInicial m@(Mapa p t) props = Jogo { mapa      = m 
                                , pista     = props
                                , carros    = map (carroInicial t (fst p)) [0..njogadores-1]
                                , nitros    = replicate njogadores qntnitro
                                , historico = replicate njogadores [(fst p)]
                                }

carroInicial :: Tabuleiro -> Posicao -> Int -> Carro
carroInicial t (a,b) i = Carro { posicao    = centroPeca tp (a,b)
                               , direcao    = 0
                               , velocidade = (0,0)
                               }
    where  (Peca tp _) = atNote "init" (atNote "init" t b) a

nitra :: Bool -> Bool -> Int -> Nitro
nitra True True lancador = Nitro [lancador]
nitra True False lancador = Puff
nitra False _ lancador = NoFuel

instance Monoid Nitro where
    mempty = NoFuel
    mappend NoFuel y = y
    mappend x NoFuel = x
    mappend Puff y = y
    mappend x Puff = x
    mappend (Nitro x) (Nitro y) = Nitro (x++y)

atualizaEMovimenta :: Tempo -> Estado -> [Acao] -> [Bool] -> Estado
atualizaEMovimenta t e a eTimeouts = {-trace ("atualizaEMovimenta " ++ show t) $-} Estado j' eBatotas eMortes eNitros eVoltas eTimeouts
    where
    eVoltas = map voltasI [0..njogadores-1]
    
    prc = percorre [] tab (fst p0) (snd p0)
    whereAreWe = map (whereAmI j' 0) [0..njogadores-1]
    voltasI n | mxp-1 == act = (1 + fst (atNote "voltas" (estadoVoltas e) n),0)
              | otherwise = (fst (atNote "voltas" (estadoVoltas e) n),nxt)
        where (nxts,mxp) = atNote "where" whereAreWe n
              act = snd (atNote "voltas" (estadoVoltas e) n)
              nxt | act `elem` nxts = act
                  | otherwise = maximumNote ("glossTempo known bug"++show n++": "++show act++" but "++show nxts) $ filter (\k -> k <= 4 + act) nxts
        
    j = estadoJogo e
    mrts = map (\(_,i,_) -> i) carrosT3
    btts = map (\(_,_,i) -> i) carrosT3
    eBatotas = (zipWith (\a b -> (max a b)-realToFrac t) (estadoBatotas e) btts)
    eMortes = (zipWith (\a b -> (max a b)-realToFrac t) (estadoMortes e) mrts)
    nitrado :: Int -> Nitro
    nitrado p = mconcat $ map (\(lancador,a) -> nitra (nitro a == Just p) ((atNote "nitros" (nitros j) lancador) > 0) lancador) (zip [0..] a)
    eNitros = map nitrado [0..njogadores-1]
    j' = jogoT4 { carros = map (\(i,_,_) -> i) carrosT3
                , historico = zipWith fhis (historico jogoT4) carrosT3 }
    fhis l (_,_,0) = l
    fhis l _       = tail l
    jogadores = length (carros j) - 1
    jogoT4    = geraJogoT4 jogadores
    carrosT3  = map movimentaT3 [0..jogadores]
    geraJogoT4 n | n<0  = j
                 | n>=0 = atualiza t (geraJogoT4 (n-1)) n (atNote "jogo" a n)
    Mapa p0 tab = mapa jogoT4
    movimentaT3 i | batota iam wil = (l 1, 0, 1.5)
                  | c == Nothing = (l 0, 1.5, 0)
                  | otherwise = (fromJust c, 0, 0)  
      where c = colide tab t (atNote "carros" (carros jogoT4) i)
            l k = lixa (mapa jogoT4) (head $ drop k (atNote "hist" (historico jogoT4) i)) (atNote "jogo" (carros jogoT4) i)
            iam = snd $ atNote "voltas" (estadoVoltas e) i
            (wil,_) = whereAmI jogoT4 0 i
        
whereAmI :: Jogo -> Int -> Int -> ([Int],Int)
whereAmI j p n = (r,length prc)
  where r = sortOn (\v -> abs $ p-v) f
        f = findIndices (\(_,i,_) -> i == head (atNote "hist" (historico j) n)) prc
        Mapa p0 tab = mapa j
        prc = percorre [] tab (fst p0) (snd p0)
           
batota :: Int -> [Int] -> Bool
batota p0 p1 = minimum (map (\j -> j-p0) (sort p1)) > 4

lixa :: Mapa -> Posicao -> Carro -> Carro
lixa (Mapa _ t) (i,j) c = c { posicao = p , velocidade = (0,0) }
  where Peca tp _ = atNote "lixa" (atNote "lixa" t j) i 
        p = centroPeca tp (i,j)

simulaTempo :: GUIArgs -> Float -> Estado -> IO (Estado,Frame)
simulaTempo guiargs t e = do
    let j = estadoJogo e
    let a f i = if (atNote "simula" (estadoBatotas e) i)+(atNote "simula" (estadoMortes e) i)>0 
                then return (parado,Nothing,False)
                else if (atNote "dead" (estadoTimeouts e) i) then return (parado,Nothing,True) else runBot (guiBot $ f guiargs) (float2Double t) j i
    (a1,err1,dead1) <- a guiP1 0 
    (a2,err2,dead2) <- a guiP2 1 
    (a3,err3,dead3) <- a guiP3 2 
    (a4,err4,dead4) <- a guiP4 3 
    let as = [a1,a2,a3,a4] 
    let errs = [err1,err2,err3,err4]
    let e' = atualizaEMovimenta (float2Double t) e as [dead1,dead2,dead3,dead4]
    return (e',estadoFrame e' errs)

simula :: Float -> GUIArgs -> Estado -> [Frame] -> IO [Frame]
simula tempo guiargs e frames | estadoFinal e || tempo <= 0 = return frames
simula tempo guiargs e frames = do
    let tick = 1 / 20
    (e',f) <- simulaTempo guiargs tick e
    simula (tempo - tick) guiargs e' (frames++[f])
    
corre :: Float -> GUIArgs -> Estado -> IO [Int]
corre tempo guiargs e | estadoFinal e || tempo <= 0 = return $ estadoPosicoes e
corre tempo guiargs e = do
    let tick = 1 / 20
    (e',f) <- simulaTempo guiargs tick e
    corre (tempo - tick) guiargs e'

tempojogo = 1 * 60

correT6 :: GUIArgs -> IO ()
correT6 guiargs = do
    let props = guiPista guiargs
    let e = estadoInicial (guiMap guiargs) props
    posicoes <- corre tempojogo guiargs e
    let bs = encode posicoes
    BS.hPut stdout bs
    hClose stdout

simulaT6 :: GUIArgs -> IO ()
simulaT6 guiargs = do
    let props = guiPista guiargs
    let e = estadoInicial (guiMap guiargs) props
    let noerrs = [Nothing,Nothing,Nothing,Nothing]
    frames <- if isNone guiargs
        then return [estadoFrame e noerrs]
        else simula tempojogo guiargs e []
    let positions = resultado $ last frames
    let bs = encode (frames,positions)
    BS.hPut stdout bs
    hClose stdout
    --print $ writeFrames (frames,positions)

dir :: Ponto -> (Peca,Posicao,Orientacao) -> Double
dir p0 (Peca t _,p,_) = snd $ componentsToArrow (p'.-.p0)
  where p' = centroPeca t p

distRad :: Int -> Int -> Int
distRad r1 r2 = ((r2-r1) + 180) `mod` 360 - 180

mapaDefault :: Mapa
mapaDefault = Mapa ((2,1),Este) tabDefault

tabDefault :: Tabuleiro
tabDefault =
  [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0
               ,Peca Lava 0,Peca Lava 0,Peca Lava 0]
  ,[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca Recta (-1)
               ,Peca (Rampa Este) (-1),Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Oeste) (-1),Peca (Curva Este) (-1),Peca Lava 0]
  ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca (Rampa Oeste) (-1)
               ,Peca Recta (-1),Peca (Rampa Oeste) (-2),Peca (Rampa Oeste) (-3),Peca (Rampa Este) (-3),Peca (Rampa Este) (-2),Peca (Curva Sul) (-1),Peca Lava 0]
  ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0
               ,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
--   00011110000009999001100999  
-- ..//>>[]<<[][]<<[]>>>><<<<\\..
-- ..\\>><<>>[]<<<<[]<<<<>>>>//..
--   00011001111009999887788999

getMapa :: Int -> Int -> Mapa
getMapa rno i = atDef mapaDefault ms modil
    where
    modil = if l == 0 then 0 else mod i l
    l = length ms
    ms = atDef [mapaDefault] mapas rno

getProps :: IO Propriedades
getProps = do
    generate $ elements propriedades





