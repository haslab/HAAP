{-# LANGUAGE GADTs, CPP, Trustworthy, TemplateHaskell, TupleSections, ViewPatterns, DeriveDataTypeable, ScopedTypeVariables #-}

module GameB where

--import Utils

--import Paths_bomberman
import qualified CodeWorld as CW
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game

--import qualified Graphics.Gloss.Juicy as Juicy

import Control.Concurrent.Async
import Control.Exception
import System.Timeout
import Control.Concurrent 
import Control.Concurrent.MVar

--import Gloss.Capture hiding (main)

import Control.Monad
import Control.DeepSeq
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Control.Monad.State (StateT(..))
import Control.Monad.IO.Class

import Data.Map (Map(..))
import Data.Set (Set(..))
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Char as Char
import Data.Word
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Dynamic
import qualified Data.Text as T

import System.Environment
--import System.Console.CmdArgs hiding (record)

import Bomberman.Engine hiding (main)
--import qualified Bomberman.AI as AI
import Text.Printf
import Text.Read hiding (Char)
import Data.List as List

import Safe

import qualified Test.QuickCheck as Quick
import Unsafe.Coerce
import JavaScript.Web.Worker.Extras

mainGameB [js2,js3,js4] = jogo $ GUIArgs Nothing GUIPlay mapa p1 p2 p3 p4 1000 False (700,700) 100 200 10 5
    where
    mapa = MGen 25 $ Just 1234
    p1 = PHuman ("P1",Nothing)
    p2 = PBot (runBot js2) ("li1g175",Nothing)
    p3 = PBot (runBot js3) ("li1g147",Nothing)
    p4 = PBot (runBot js4) ("li1g100",Nothing)

-- * Parâmetros variáveis

guiTickFrames :: GUIArgs estado -> Int
guiTickFrames args = guiFrameRate args `div` guiTicksPerSec args
guiFrameToTick :: GUIArgs estado -> Int -> Int
guiFrameToTick args x = x `div` guiTickFrames args

data GUIMode = GUIRecord | GUIPlay
	deriving Show

data GUIArgs estado = GUIArgs
    { guiRecord :: Maybe FilePath
	, guiMode :: GUIMode
    , guiMap :: GUIMap
    , guiP1 :: GUIPlayer estado
    , guiP2 :: GUIPlayer estado
    , guiP3 :: GUIPlayer estado
    , guiP4 :: GUIPlayer estado
    , guiGameTime :: Int
    , guiNormalText :: Bool
    , guiMaxXY :: (Int,Int)
    , guiImageSize :: Int
    , guiInfoSize :: Int
    , guiFrameRate :: Int -- frames per second
    , guiTicksPerSec :: Int -- ticks per second
    }
  deriving (Show)

data GUIMap = MGen Int (Maybe Int) | MFile FilePath
  deriving (Show)

guiMapSize :: GUIMap -> Int
guiMapSize (MGen sz _) = sz

-- name and avatar number
type PlayerId = (String,Maybe Word8)

data GUIPlayer estado
    = PBot (Bot estado) PlayerId
    | PHuman PlayerId
    | PNone
  deriving (Show)

--guiPlayerEstado :: GUIPlayer estado -> IO estado
--guiPlayerEstado (PBot b _) = iniBot bot
--guiPlayerEstado PHuman = return mempty
--guiPlayerEstado PNone = return mempty

guiPlayerName :: GUIPlayer estado -> String
guiPlayerName (PBot _ (n,_)) = n
guiPlayerName (PHuman (n,_)) = n

parseGUIMap :: GUIMap -> IO Mapa
parseGUIMap guimap = do
    str <- case guimap of
        MGen dim seed -> do
            seed' <- maybe (Quick.generate Quick.arbitrary) return seed
            --putStrLn $ "generated map " ++ show dim ++ " " ++ show seed'
            return $ unlines0 $ tarefa1 dim seed' maxBound
        MFile file -> readFile file
    case leMapa str of
        Left err -> Prelude.error (show err) -- panic!
        Right mapa -> do
            --putStrLn $ str
            --putStrLn $ show mapa
            return mapa

parseGUIPlayer :: GUIPlayer estado -> IO (Maybe (PlayerData estado))
parseGUIPlayer PNone = return Nothing
parseGUIPlayer (PHuman (n,Just i)) = do
    pict <- loadPlayerBMP i
    return $ Just $ PlayerData (Right ()) n pict Nothing (Prelude.error "human")
parseGUIPlayer (PBot bot (n,Just i)) = do
    pict <- loadPlayerBMP i
    ini <- initBot bot
    return $ Just $ PlayerData (Left bot) n pict Nothing ini

parseGUIPlayers :: GUIArgs estado -> IO (Map Word8 (PlayerData estado))
parseGUIPlayers args = do
    [arg1,arg2,arg3,arg4] <- genPlayerAvatars [guiP1 args,guiP2 args,guiP3 args,guiP4 args]
    p1 <- parseGUIPlayer $ arg1
    p2 <- parseGUIPlayer $ arg2
    p3 <- parseGUIPlayer $ arg3
    p4 <- parseGUIPlayer $ arg4
    return $ Map.map (fromJustNote "parseGUIPlayers") $ Map.filter isJust $ Map.fromList $ zip [0..] [p1,p2,p3,p4]

addMapPlayers :: Mapa -> Map Word8 (PlayerData estado)-> IO (Mapa,Map Word8 (PlayerData estado))
addMapPlayers m ps = do
    let m' = foldr aux1 m $ Map.toList ps
    ps' <- foldM aux3 ps $ Map.toList $ players m
    return (m',ps')
  where
    dim = boardDim $ board m
    aux1 (p,dta) m = m { players = Map.alter (aux2 p) p $ players m }
    aux2 p (Just pst) = Just pst
    aux2 0 Nothing = Just $ defPStat $ Pos 1 1
    aux2 1 Nothing = Just $ defPStat $ Pos (dim-2) 1
    aux2 2 Nothing = Just $ defPStat $ Pos (dim-2) (dim-2)
    aux2 3 Nothing = Just $ defPStat $ Pos 1 (dim-2)
    aux3 ps (p,pst) = do
        case Map.lookup p ps of
            Just x -> return ps
            Nothing -> defPData >>= \dta -> return $ Map.insert p dta ps

-- * Bots

type MyBot = [String] -> Int -> Int -> Maybe Char

--parado :: Bot estado
--parado = Bot $ \xs i j estado -> return (Nothing,estado)

runBot :: String -> Bot (SyncWorker ([String],Int,Int) (Maybe Char))
runBot workerfile = Bot (newSyncWorker workerfile) go
    where
    go mapa player ticks worker = do
        tryPutSyncWorker worker (inp::[String],player::Int,ticks::Int)
        mbres <- tryTakeSyncWorker worker
        case mbres of
            Nothing -> do
                return (Nothing,worker)
            Just jogada -> return (jogada,,worker)
    
asyncTimeout_ :: Int -> IO a -> IO (Maybe a)
asyncTimeout_ i f =
  withAsync f $ \a1 ->
  withAsync (threadDelay i) $ \a2 ->
  liftM (either Just (const Nothing)) $ race (wait a1) (wait a2)

mkBot :: IO estado -> ([String] -> Int -> Int -> estado -> (Maybe Char,estado)) -> Bot estado
mkBot ini f = Bot ini $ \m player ticks estado -> return $ f (prettyMapa m) player ticks estado

-- estado
data Bot estado = Bot { initBot :: IO estado, unBot :: Mapa -> Int -> Int -> estado -> IO (Maybe Char,estado)}
instance Show (Bot estado) where
    show _ = "<bot>"

-- * Gloss

loadGraphic :: FilePath -> IO Picture
loadGraphic file = do
    loadImageById ("graphics/" ++ file ++".bmp")

loadPlayerBMP :: Word8 -> IO Picture
loadPlayerBMP i = loadGraphic $ "players/p" ++ show i

loadExplosionBMP :: Word8 -> IO Picture
loadExplosionBMP i = loadGraphic $ "explosions/e" ++ show i

loadFontBMP :: Char -> IO Picture
loadFontBMP c = loadGraphic $ "fonts/" ++ [Char.toLower c]

data PlayerData estado = PlayerData { pMove :: Either (Bot estado) (), pName :: String, pAvatar :: Picture, pDead :: Maybe Int, pState :: estado }
  deriving (Show)

genPlayerAvatars :: [GUIPlayer estado] -> IO [GUIPlayer estado]
genPlayerAvatars ps = aux ps (concatMap avatar ps)
    where
    avatar (PBot _ (n,Just w)) = [w]
    avatar (PHuman (n,Just w)) = [w]
    avatar _ = []
    aux [] _ = return []
    aux (PBot bot (n,Nothing):ps) acc = do
        let base::[Word8] = [1..28]\\acc
        i <- Quick.generate $ Quick.elements base
        ps' <- aux ps (i:acc)
        return (PBot bot (n,Just i):ps')
    aux (PHuman (n,Nothing):ps) acc = do
        let base::[Word8] = [1..28]\\acc
        i <- Quick.generate $ Quick.elements base
        ps' <- aux ps (i:acc)
        return (PHuman (n,Just i):ps')
    aux (p:ps) acc = do
        ps' <- aux ps acc
        return (p:ps')

defPData :: IO (PlayerData estado)
defPData = do
    i <- Quick.generate $ Quick.choose (1,28)
    pict <- loadPlayerBMP i
    return $ PlayerData (Right ()) "???" pict Nothing (Prelude.error "defPData")

-- | função principal
jogo :: GUIArgs estado -> IO ()
jogo guiargs = do
	mapa0 <- parseGUIMap $ guiMap guiargs
	players0 <- parseGUIPlayers guiargs
	(mapa,players) <- addMapPlayers mapa0 players0
	let (janela,bloco) = tamanhos guiargs mapa
	let maxtime = guiGameTime guiargs
	parede <- loadGraphic "parede"
	tijolo <- loadGraphic "tijolo"
	bombs <- loadGraphic "bombs" 
	flames <- loadGraphic "flames" 
	let powerup i = case i of
						Bombs -> bombs
						Flames -> flames
	bomba <- loadGraphic "bomb"
	explosions <- mapM loadExplosionBMP [0..6]
	let explosion i = explosions!!i
	let gloss = MapaGloss (mapa,[]) janela bloco parede tijolo powerup bomba explosion (maxtime * guiFrameRate guiargs) (guiTickFrames guiargs) players (guiNormalText guiargs)
	case guiMode guiargs of
		GUIPlay -> joga guiargs gloss
		GUIRecord -> grava guiargs gloss (guiRecord guiargs)

grava :: GUIArgs estado -> MapaGloss estado -> Maybe FilePath -> IO ()
grava guiargs gloss Nothing = do
    gloss' <- corre guiargs gloss
    putStrLn $ unlines $ map show $ getPlayerRank $ playerData gloss'
    return ()

getPlayerRank :: Map Word8 (PlayerData estado) -> [(Word8,Int)]
getPlayerRank xs = genRanks 1 $ sortBy cmp $ Map.toList xs
    where
    cmp :: (Word8,PlayerData estado) -> (Word8,PlayerData estado) -> Ordering
    cmp x y = compare (f x) (f y)
    f :: (Word8,PlayerData estado) -> Int
    f (x,dta) = maybe minBound id (pDead dta)
    genRanks :: Int -> [(Word8,PlayerData estado)] -> [(Word8,Int)]
    genRanks i [] = []
    genRanks i (x:xs) = let (wins,loses) = partition (\y -> f x == f y) xs in map ((,i) . fst) (x:wins) ++ genRanks (succ i + length wins) loses 

corre :: GUIArgs estado -> MapaGloss estado -> IO (MapaGloss estado)
corre guiargs gloss = do
    gloss' <- if fim gloss
        then return gloss
        else do
			gloss' <- reageTempo guiargs 1 gloss
			corre guiargs gloss'
    return (gloss')

fim :: MapaGloss estado -> Bool
fim gloss = (framesTillEnd gloss <= 0)
         || (Map.size (players $ fst $ estado gloss) <= 1)

-- | Estado do jogo:
data MapaGloss estado = MapaGloss {
      estado :: (Mapa,Burned) -- estado do jogo (mapa,chamas)
    , janela :: (Int,Int) -- tamanho da janela
    , bloco :: Int -- tamanho de cada célula
    , paredeBMP :: Picture -- sprite da parede fixa
    , tijoloBMP :: Picture -- sprite do tijolo
    , powerupBMP :: PowerUp -> Picture -- sprite de powerups
    , bombaBMP :: Picture -- sprite de uma bomba
    , explosionBMP :: Int -> Picture -- sprite de uma explosão
    , framesTillEnd :: Int
    , framesTillTick :: Int
    , playerData :: Map Word8 (PlayerData estado)
    , textStyle :: Bool
    }

playerBMP :: MapaGloss estado -> Word8 -> Picture
playerBMP gloss p = case Map.lookup p (playerData gloss) of
    Nothing -> Prelude.error $ "player " ++ show p ++ " not found"
    Just dta -> pAvatar dta

-- | Desenha o jogo dentro da janela
desenhaMapa :: GUIArgs estado -> MapaGloss estado -> IO Picture
desenhaMapa guiargs gloss = do
    let box = Color black $ Polygon [(-x2,-y2),(-x2,y2),(x2,y2),(x2,-y2)]
    let infobox = Translate (x2-info2) 0 $ Color color $ Polygon [(0,y2),(info,y2),(info,-y2),(0,-y2)]
    let pict = Translate (-info2) 0 $ Pictures $ box : desenhaTabuleiro guiargs gloss (estado gloss)
    
    -- time box
    let secondsTillEnd = framesTillEnd gloss `div` guiFrameRate guiargs
    let secondsTillEndAbs = if secondsTillEnd < 0 then 0 else secondsTillEnd
    timetext <- desenhaTexto (textStyle gloss) $ printf "%03d" secondsTillEndAbs
    let timecolor = if guiFrameToTick guiargs (framesTillEnd gloss) <= (fromEnum dim-1)^2 then red else black
    let timebox = Color timecolor $ Translate (x2-info2+45) (y2/2+100) $ Scale (0.5) (0.5) timetext
    
    ps <- players
    return $ Pictures [infobox,pict,timebox,ps,Pictures mapInfo]
  where
    dim = boardDim $ board $ fst $ estado gloss
    (x,y) = janela gloss
    x2 = fromIntegral x / 2
    y2 = fromIntegral y / 2
    info = fromIntegral (guiInfoSize guiargs)
    info2 = info / 2
    color = makeColorI 255 178 102 150
    color2 = makeColorI 64 23 15 255
    players = do
        ps <- mapM player $ Map.toList $ playerData gloss
        return $ Translate (x2-info2+45) (y2/2+50) $ Pictures ps
    player (i,dta) = do
        pname <- desenhaTexto (textStyle gloss) $ pName dta
        return $ Translate 0 (-50*fromIntegral i) $ Pictures [pcell i dta,Translate 40 (-10) $ Pictures [dead i, Scale (0.2) (0.2) pname]]
    pcell i dta = drawCell guiargs gloss $ Pictures [Color color2 $ Polygon [(-70,-70),(-70,70),(70,70),(70,-70)],pAvatar dta]
    dead i = case fmap pDead (Map.lookup i $ playerData gloss) of
        Just Nothing -> Color black $ Line [(-5,-10),(-5,25),(87,25),(87,-10),(-5,-10)]
        Just (Just _) -> Color black $ Line [(-5,7),(87,7)]
        Nothing -> Blank
    mapInfo =
        [Translate (x2-info2+15) (y2/2-180) $ Color color2 $ Scale 0.1 0.1 $ Text "Keys:"
        ,Translate (x2-info2+15) (y2/2-200) $ Color color2 $ Scale 0.1 0.1 $ Text "Movement (W,S,A,D)"
        ,Translate (x2-info2+15) (y2/2-220) $ Color color2 $ Scale 0.1 0.1 $ Text "Bomb (Q)"
        ]

desenhaTexto :: Bool -> String -> IO Picture
desenhaTexto True s = return $ Scale (0.2) (0.2) $ Text s
desenhaTexto False s = do
    picts <- desenhaTexto' s 0
    return $ Translate 30 30 $ Pictures picts
    where
    desenhaTexto' [] off = return []
    desenhaTexto' (c:cs) off = do
        p@(Image width _ _) <- loadFontBMP c
        ps <- desenhaTexto' cs (off+width+2)
        return (Translate (fromIntegral off) 0 p:ps)

desenhaTabuleiro :: GUIArgs estado -> MapaGloss estado -> (Mapa,Burned) -> [Picture]
desenhaTabuleiro guiargs gloss (m,burned) = [pb,pfs,Pictures pbs,ppws,ppls]
    where
    ppls = drawPlayers guiargs gloss (players m)
    ppws = drawPowerUps guiargs gloss (powers m)
    pb = drawBoard guiargs gloss (board m)
    pfs = paintFlames guiargs gloss burned
    pbs = map drawPBomb $ Map.toList $ allBombsWithPlayer m
    drawPBomb (p,(i,bmb)) = drawPos guiargs gloss p $ bombaBMP gloss

paintFlames :: GUIArgs estado -> MapaGloss estado -> Burned -> Picture
paintFlames guiargs gloss = Pictures . map paintFlame
    where
    paintFlame (p,i) = drawPos guiargs gloss p (explosionBMP gloss i)

drawBoard :: GUIArgs estado -> MapaGloss estado -> Board -> Picture
drawBoard guiargs gloss board = Pictures $ drawTijolos (boardTijolos board) ++ drawParedes
    where
    nparedes = boardCaracol board
    dim = boardDim board
    closed = closeParedes dim nparedes
    drawTijolos = map drawTijolo
    drawTijolo p = drawPos guiargs gloss p $ tijoloBMP gloss
    paredesfixas = [Pos x y | y <- [0..dim-1], x <- [0..dim-1], isWall dim (Pos x y) closed]
    drawParedes = map drawParede (paredesfixas ++ closeParedes dim nparedes)
    drawParede p = drawPos guiargs gloss p $ paredeBMP gloss

drawPlayers :: GUIArgs estado -> MapaGloss estado -> Players -> Picture
drawPlayers guiargs gloss = Pictures . map drawPlayer . Map.toList
    where
    drawPlayer (p,pstat) = drawPos guiargs gloss (pPos pstat) (playerBMP gloss p)

drawPowerUps :: GUIArgs estado -> MapaGloss estado -> PowerUps -> Picture
drawPowerUps guiargs gloss = Pictures . map drawPowerUp . Map.toList
    where
    tijolos = boardTijolos $ board $ fst $ estado gloss
    drawPowerUp (p,pw) = if List.elem p tijolos then Blank else drawPos guiargs gloss p $ powerupBMP gloss pw

drawPos :: GUIArgs estado -> MapaGloss estado -> Pos -> Picture -> Picture
drawPos guiargs gloss (Pos x y) pict = Translate px py $ drawCell guiargs gloss pict
    where
    b = bloco gloss
    (pX,pY) = janela gloss
    px = toEnum (fromIntegral x * b) - (fromIntegral pX / 2) + (toEnum b / 2)
    py = (fromIntegral pY / 2) - toEnum (fromIntegral y * b) - (toEnum b / 2)

drawCell :: GUIArgs estado -> MapaGloss estado -> Picture -> Picture
drawCell guiargs gloss pict =  Scale s s pict
    where
    b = bloco gloss
    s = toEnum (bloco gloss) / toEnum (guiImageSize guiargs)

-- | Reage ao pressionar das setas do teclado, movendo a bola 5 pixéis numa direção
reageEvento :: GUIArgs estado -> Event -> MapaGloss estado -> IO (MapaGloss estado)
reageEvento guiargs e gloss = case eventToMovimento e of
    Just (p,Down,k) -> return $ gloss { estado = let (x,y) = estado gloss in (moveMapaPerfect 10 x p k,y) }
    otherwise -> return gloss

-- (player,movimento)
eventToMovimento :: Event -> Maybe (Word8,KeyState,Char)
eventToMovimento (EventKey (Char 'w')               st _ _) = Just (0,st,'U')
eventToMovimento (EventKey (Char 's')               st _ _) = Just (0,st,'D')
eventToMovimento (EventKey (Char 'a')               st _ _) = Just (0,st,'L')
eventToMovimento (EventKey (Char 'd')               st _ _) = Just (0,st,'R')
eventToMovimento (EventKey (Char 'q')               st _ _) = Just (0,st,'B')
eventToMovimento (EventKey (Char 't')               st _ _) = Just (1,st,'U')
eventToMovimento (EventKey (Char 'g')               st _ _) = Just (1,st,'D')
eventToMovimento (EventKey (Char 'f')               st _ _) = Just (1,st,'L')
eventToMovimento (EventKey (Char 'h')               st _ _) = Just (1,st,'R')
eventToMovimento (EventKey (Char 'y')               st _ _) = Just (1,st,'B')
eventToMovimento (EventKey (Char 'i')               st _ _) = Just (2,st,'U')
eventToMovimento (EventKey (Char 'k')               st _ _) = Just (2,st,'D')
eventToMovimento (EventKey (Char 'j')               st _ _) = Just (2,st,'L')
eventToMovimento (EventKey (Char 'l')               st _ _) = Just (2,st,'R')
eventToMovimento (EventKey (Char 'o')               st _ _) = Just (2,st,'B')
eventToMovimento (EventKey (SpecialKey KeyUp)       st _ _) = Just (3,st,'U')
eventToMovimento (EventKey (SpecialKey KeyDown)     st _ _) = Just (3,st,'D')
eventToMovimento (EventKey (SpecialKey KeyLeft)     st _ _) = Just (3,st,'L')
eventToMovimento (EventKey (SpecialKey KeyRight)    st _ _) = Just (3,st,'R')
eventToMovimento (EventKey (SpecialKey KeySpace)   st _ _) = Just (3,st,'B')
eventToMovimento _ = Nothing

data DynState where
	DynState :: a -> DynState

instance Show DynState where
	show (DynState _) = "<DynState>"
instance NFData DynState where
	rnf = (`seq` ())

killPlayersData :: [Word8] -> Int -> Map Word8 (PlayerData estado) -> Map Word8 (PlayerData estado)
killPlayersData [] time m = m
killPlayersData (x:xs) time m = killPlayersData xs time (Map.update (Just . killPlayerData) x m)
    where
    killPlayerData :: PlayerData estado -> PlayerData estado
    killPlayerData pdta = pdta { pDead = Just $ maybe time id (pDead pdta) }

-- | Reage ao tempo.
reageTempo :: GUIArgs estado -> Int -> MapaGloss estado -> IO (MapaGloss estado)
reageTempo guiargs passedframes gloss = if fim gloss
    then return $ gloss { framesTillEnd = framesTillEnd gloss - passedframes }
    else do
        ((estado',dead'),pdta') <- State.runStateT (chgPlayers $ estado gloss) (playerData gloss)
        let (estado'',dead'') = chgTime $ estado'
        return $ gloss
            { framesTillEnd = framesTillEnd gloss - passedframes
            , framesTillTick = if frames==0 then guiTickFrames guiargs else frames
            , estado = estado''
            , playerData = killPlayersData (dead'++dead'') ticksTillEnd pdta' }
  where
    ticksTillEnd = guiFrameToTick guiargs $ framesTillEnd gloss
    frames = pred (framesTillTick gloss)
    chgTime :: (Mapa,Burned) -> ((Mapa,Burned),[Word8])
    chgTime st = if frames/=0 then (st,[]) else Reader.runReader (avancaTempo st ticksTillEnd) perfect
    chgPlayers :: (Mapa,Burned) -> StateT (Map Word8 (PlayerData estado)) IO ((Mapa,Burned),[Word8])
    chgPlayers (x,y) = if frames /=0 then return ((x,y),[]) else do
        let (x',dead) = Reader.runReader (cleanseMap (map fst y) [] x) perfect
        pdata <- State.get
        (x'',dead') <- foldM chgPlayer (x',dead) $ Map.toList pdata
        return ((x'',[]),dead')
    chgPlayer :: (Mapa,[Word8]) -> (Word8,PlayerData estado) -> StateT (Map Word8 (PlayerData estado)) IO (Mapa,[Word8])
    chgPlayer (m,dead) (p,dta) = do
        let (x,y) = estado gloss
        --let pos = fmap pPos $ Map.lookup p $ players x
        case pMove dta of
            Left (Bot ini bot) -> do
                case Map.lookup p (players m) of
                    Nothing -> return (m,dead) -- do not call the bot if the player does not exist
                    Just pst -> do
                        mb <- State.lift $ eitherFail botTimeout $ bot (removeHiddenPowerUps m) (fromEnum p) ticksTillEnd (pState dta)
                        case mb of
                            Left err -> State.lift $ putStrLn $ show p ++ " " ++ show (pName dta) ++ " " ++ show err
                            otherwise -> return ()
                        let (mv,st') = case mb of
                                            Left err -> (Just 'e',pState dta)
                                            Right (mv,st) -> (mv,st)
                        let dta' = dta { pState = st' }
                        State.modify $ \psst -> Map.insert p dta' psst
                        case mv of
                            Nothing -> return (m,dead)
                            Just mv -> do
                                unless (List.elem mv "UDLRB") $ State.lift $ putStrLn $ show p ++ " " ++ show (pName dta) ++ " unknown move " ++ show mv
                                let (m',dead') = moveMapaPerfectBot 10 m p mv
                                return (m',dead++dead')
            Right () -> return (m,dead)

-- bot maximum time per play, in seconds
botTimeout :: Maybe Float
botTimeout = Just 2

-- | tamanho de janela e blocos dinâmicos
tamanhos :: GUIArgs estado -> Mapa -> ((Int,Int),Int)
tamanhos guiargs mapa = ((fromIntegral $ blockX * x,fromIntegral $ blockX * y),min blockX blockY)
    where
    tab = board mapa
    x = fromIntegral $ boardDim tab
    y = fromIntegral $ boardDim tab
    sizeX = min (x * guiImageSize guiargs) (fst $ guiMaxXY guiargs)
    sizeY = min (y * guiImageSize guiargs) (snd $ guiMaxXY guiargs)
    blockX0 = sizeX `div` x
    blockY0 = sizeY `div` y
    blockX = if odd blockX0 then blockX0+1 else blockX0
    blockY = if odd blockY0 then blockY0+1 else blockY0

-- | Inicia um jogo a partir de um mapa inicial
joga :: GUIArgs estado -> MapaGloss estado -> IO ()
joga guiargs mapaInicial = do
    let (x,y) = (janela mapaInicial)
    screen <- getDisplay
    let window = Display (x+fromIntegral (guiInfoSize guiargs)) (y)
    playFitScreenIO screen window
        --(InWindow "Bomberman" window (0, 0)) -- Tamanho da janela do jogo
        (white) -- Côr do fundo da janela
        (guiFrameRate guiargs) -- refresh rate
        mapaInicial -- mapa inicial
        (desenhaMapa guiargs) -- função que desenha o mapa
        (reageEvento guiargs) -- função que reage a um evento (carregar numa tecla, mover o rato, etc)
        (\f m -> reageTempo guiargs 1 m) -- função que reage ao passar do tempo


mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (x,y) = (x,f y) 

eitherFail :: Maybe Float -> IO a -> IO (Either SomeException a)
eitherFail cargs m = catch (liftM Right $ m) (\(e::SomeException) -> return $ Left e)

unlines0 :: [String] -> String
unlines0 [] = ""
unlines0 [x] = x
unlines0 (x:xs) = x ++ "\n" ++ unlines0 xs

