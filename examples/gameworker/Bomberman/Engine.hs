{-# LANGUAGE DeriveGeneric, FlexibleContexts, ScopedTypeVariables, TupleSections, ViewPatterns #-}

module Bomberman.Engine where

import Control.Applicative ((<*))
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State (State(..))
import Control.Monad.State as State
import Text.Parsec hiding (between,State)
import Text.Parsec.Error
import Data.List as List
import System.Random
import Data.Word
import Data.Maybe
--import Utils
--import Control.DeepSeq
--import Control.DeepSeq.Generics 

import qualified GHC.Generics as GHC

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Control.Monad.Identity
import Control.Monad.Reader (Reader (..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer

--instance NFData Bomb where
--    rnf = genericRnf
--instance NFData Board where
--    rnf = genericRnf
--instance NFData PStat where
--    rnf = genericRnf
--instance NFData Pos where
--    rnf = genericRnf
--instance NFData PowerUp where
--    rnf = genericRnf
--instance NFData Mapa where
--    rnf = genericRnf

-- * Tarefa 1

equalsTabs :: String -> String -> Bool
--equalsTabs x y = case (leMapa x,leMapa y) of
--    (Right x',Right y') -> x' == y'
--    otherwise -> False
equalsTabs x y = tabx == taby && map words xs == map words ys
    where
    (tabx,xs) = span (isPrefixOf "#") (mylines x)
    (taby,ys) = span (isPrefixOf "#") (mylines y)
    
mylines = filter (/="") . lines

data St = St { rand :: StdGen, bombs :: [Pos], flames :: [Pos] }

tarefa1 :: Int -> Int -> Int -> [String]
tarefa1 dim seed ticksToEnd = flip evalState (St (mkStdGen seed) [] []) $ do
  let d = toEnum dim
  tab <- tabuleiro d (closeParedes d $ closedBlocks d ticksToEnd)
  st <- get
  let showpos c (Pos x y) = c : " " ++ show x ++ " " ++ show y
  let bs = map (showpos '+') (bombs st)
  let fs = map (showpos '!') (flames st)
  return (tab++bs++fs)

tabuleiro :: Word8 -> [Pos] -> State St [String]
tabuleiro dim closed = forM [0..dim-1] (linha dim closed)

linha :: Word8 -> [Pos] -> Word8 -> State St String
linha dim closed y = forM [0..dim-1] $ \x -> posicao dim (Pos x y) closed

posicao :: Word8 -> Pos -> [Pos] -> State St Char
posicao dim p closed | isWall dim p closed = return '#'
posicao dim p closed | isCorner dim p = return ' '
posicao dim pos closed = do
  st <- get
  let (i,g') = randomR (0,99) (rand st)
  put st{rand = g'}
  gera pos i

gera :: Pos -> Int -> State St Char
gera pos i = case i of
  (between 0 1 -> True) -> do
    st <- get
    put st{bombs = bombs st ++ [pos]}
    return '?'
  (between 2 3 -> True) -> do
    st <- get
    put st{flames = flames st ++ [pos]}
    return '?'
  (between 4 39 -> True) -> return '?'
  (between 40 99 -> True) -> return ' '

-- * Estado

data Pos = Pos Word8 Word8
  deriving (Eq,Show,GHC.Generic)
instance Ord Pos where
    compare (Pos x1 y1) (Pos x2 y2) = compare (y1,x1) (y2,x2)
    
defPStat :: Pos -> PStat
defPStat p = PStat p 1 1

allBombs :: Mapa -> Map Pos Bomb
allBombs mapa = Map.unions $ Map.elems $ mBombs mapa

removeHiddenPowerUps :: Mapa -> Mapa
removeHiddenPowerUps m = m { powers = Map.filterWithKey (\k v -> isVisible k) $ powers m }
    where
    isVisible p = not $ List.elem p (boardTijolos $ board m)

data PowerUp = Bombs | Flames
  deriving (Eq,Show,GHC.Generic)
type PowerUps = Map Pos PowerUp
data PStat = PStat { pPos :: Pos, pConstip :: Word8, pRadius :: Word8 }
  deriving (Show,GHC.Generic)
type Players = Map Word8 PStat
data Bomb = Bomb { bRadius :: Word8, bTime :: Word8 }
  deriving (Show,GHC.Generic)
type Bombs = Map Pos Bomb
data Mapa = Mapa { board :: Board, players :: Players, powers :: PowerUps, mBombs :: Map Word8 Bombs }
  deriving (Show,GHC.Generic)
data Board = Board { boardDim :: Word8, boardTijolos :: [Pos], boardCaracol :: Word16 }
  deriving (Show,GHC.Generic)

-- 0 -> core
-- 1 -> top
-- 2 -> down
-- 3 -> left
-- 4 -> right
-- 5 -> top tip
-- 6 -> down tip
-- 7 -> left tip
-- 8 -> right tip

emptyPStat = PStat (Pos undefined undefined) undefined undefined

emptyMapa = Mapa undefined Map.empty Map.empty Map.empty

allBombPlayerPowerPositions :: Mapa -> Set Pos
allBombPlayerPowerPositions m = Set.unions [Map.keysSet $ allBombs m,Set.fromList $ map pPos $ Map.elems $ players m,Map.keysSet $ powers m]

--isMove :: Mapa -> Pos -> Bool
--isMove m p@(Pos x y) = x < dim-1 && y < dim-1 && not (isWall dim p [] || isTijolo m p)
--    where
--    dim = boardDim $ board m

isEdge :: Word8 -> Pos -> Bool
isEdge dim (Pos x y) = x == 0 || y == 0 || x == dim-1 || y == dim-1

isCorner :: Word8 -> Pos -> Bool
isCorner dim (Pos x y) = (x == 1 && (between 1 2 y || between (dim-3) (dim-2) y))
                  || (y == 1 && (between 1 2 x || between (dim-3) (dim-2) x))
                  || (x == dim-2 && (between 1 2 y || between (dim-3) (dim-2) y))
                  || (y == dim-2 && (between 1 2 x || between (dim-3) (dim-2) x))

isWall :: Word8 -> Pos -> [Pos] -> Bool
isWall dim p@(Pos x y) ps = isEdge dim (Pos x y) || even x && even y || List.elem p ps

isTijolo :: Mapa -> Pos -> Bool
isTijolo m p = List.elem p (boardTijolos $ board m)

isPowerUp :: Mapa -> Pos -> Bool
isPowerUp m p = isJust $ Map.lookup p (powers m)

isOpaque :: MonadReader Mutation m => Mapa -> Pos -> m Bool
isOpaque m p = do
  mut <- Reader.ask
  let isBomb = case mut of
          Perfect True -> isJust (Map.lookup p $ allBombs m)
          otherwise -> False
  return (isTijolo m p || isPowerUp m p || isBomb)

-- * Parser

parseNew :: P ()
parseNew = newline >> return ()
parseEnd = many parseNew <* eof

leMapa :: String -> Either ParseError Mapa
leMapa = runParser (parseMapa <* parseEnd) (emptyMapa) "" . (++"\n")

type P = Parsec String Mapa

pMany :: P a -> P [a]
pMany p = pMaybeCont p $ \mb -> case mb of
    Nothing -> return []
    Just x -> do
        xs <- pMany p
        return (x:xs)

pMaybeCont :: P a -> (Maybe a ->P b) -> P b
pMaybeCont p cont = (p >>= cont . Just) <||> cont Nothing

infixr 1 <||>
(<||>) :: P a -> P a -> P a
p1 <||> p2 = try p1 <|> p2

pManyNews :: P a -> P [a]
pManyNews p = pMany (p >>= \x -> parseNew >> return x) --sepBy p parseNew

--ltPos :: Pos -> Pos -> Bool
--ltPos (x,y) (Pos x' y') | y == y' = x <= x'
--                    | y > y' = False
--                    | y < y' = True 

parseMapa :: P Mapa
parseMapa = do
    parseBoard
    parseNew
    bombs <- pManyNews (parsePowerUp '+' Bombs)
    unless (isSorted bombs) $ unexpected $ "powerups Bombs não ordenadas por posição " ++ show bombs
    flames <- pManyNews (parsePowerUp '!' Flames)
    unless (isSorted flames) $ unexpected $ "powerups Flames não ordenadas por posição " ++ show flames
    (stars,starps) <- liftM unzip $ pManyNews (parseStar)
    unless (isSorted stars) $ unexpected $ "bombas não ordenadas por posição " ++ show stars
    ps <- pManyNews (parsePlayer)
    getState

parsePowerUp :: Char -> PowerUp -> P Pos
parsePowerUp c pw = do
    char c
    parseSpaces1
    p <- parsePos False True True
    modifyState $ \m -> m { powers = Map.insert p pw (powers m) }
    return p

parseStar :: P (Pos,Word8)
parseStar = do
    char '*'
    parseSpaces1
    p <- parsePos True True True
    parseSpaces1
    i <- parsePId
    parseSpaces1
    r <- parseWord8
    parseSpaces1
    t <- parseWord8
    let addBomb xs = Map.insert p (Bomb r t) xs
    modifyState $ \m -> m { mBombs = Map.alter (Just . addBomb . maybe Map.empty id) i (mBombs m) }
    return (p,i)

parsePlayers :: P ()
parsePlayers = do
    is <- pManyNews (parsePlayer)
    unless (isSorted is) $ unexpected $ "jogadores não ordenados por identificador"

parsePId :: P Word8
parsePId = do
    i <- parseWord8
    unless (between 0 3 i) $ unexpected $ "jogador " ++ show i ++ " inválido"
    return i

parsePlayer :: P Word8
parsePlayer = do
    i <- parsePId
    parseSpaces1
    p <- parsePos True True False
    parseSpaces
    c <- liftM (toEnum . length) $ many (char '+')
    r <- liftM (toEnum . length) $ many (char '!')
    let upd st = st { pPos = p, pConstip = succ c, pRadius = succ r }
    modifyState $ \m -> m { players = Map.alter (Just . upd . maybe emptyPStat id) i (players m) }
    return i

--checkRadius :: Word8 -> P ()
--checkRadius r = do
--    st <- getState
--    let dim = boardDim st
--    unless (r < dim) $ unexpected "raio não pode ser maior do que a dimensão da área de jogo"

parseParede = char '#'

parseSpace :: P Char
parseSpace = char '\t' <|> char ' '

parseSpaces1 = many1 parseSpace
parseSpaces = many parseSpace

parseWord :: P Word
parseWord = liftM read (many1 digit)

parseWord8 :: P Word8
parseWord8 = liftM read (many1 digit)

parsePos :: Bool -> Bool -> Bool -> P Pos
parsePos isVisible checkPower checkBomb = do
    x <- parseWord8
    parseSpaces1
    y <- parseWord8
    notWall isVisible (Pos x y)
    notPower checkPower (Pos x y)
    return (Pos x y)

notWall :: Bool -> Pos -> P ()
notWall isVisible p = do
    tab <- liftM (board) getState
    let dim = boardDim tab
    when (not isVisible) $ when (isCorner dim p) $ unexpected $ "posição não deve ser canto"
    case lookPos tab p [] of
        Just '#' -> unexpected $ "posição " ++ show p ++ " não deve ser parede"
        Just '?' -> when isVisible $ unexpected $ "posição " ++ show p ++ " não deve ser tijolo"
        otherwise -> return ()
        
notPower :: Bool -> Pos -> P ()
notPower checkPower p = do
    pws <- liftM (powers) getState
    case Map.lookup p pws of
        Just _ -> when checkPower $ unexpected $ "posição " ++ show p ++ " não deve ser powerup"
        otherwise -> return ()

notBomb :: Bool -> Pos -> P ()
notBomb checkBomb p = do
    bbs <- liftM (allBombs) getState
    case Map.lookup p bbs of
        Just _ -> when checkBomb $ unexpected $ "posição " ++ show p ++ " não deve ser bomba"
        otherwise -> return ()

parseBoard :: P ()
parseBoard = do
    x <- many parseParede
    let n::Word8 = toEnum $ length x -- verifica o tamanho igual de todas as linhas
    (xs) <- replicateM (fromEnum $ n-1) (parseNew >> parseLinhaMeio n)
    let (tijs,walls) = parseBoardTijolos (Pos 0 0) (x:xs)
    caracol <- validaCaracol n walls
    let b = Board n tijs caracol
    validaBoard n b
    modifyState $ \m -> m { board = b }

parseBoardTijolos :: Pos -> [String] -> ([Pos],[Pos])
parseBoardTijolos p [] = ([],[])
parseBoardTijolos p@(Pos x y) (l:ls) = (xs1++xs2,ys1++ys2)
    where
    (xs1,ys1) = parseBoardTijolos' p l
    (xs2,ys2) = parseBoardTijolos (Pos 0 $ succ y) ls
parseBoardTijolos' :: Pos -> String -> ([Pos],[Pos])
parseBoardTijolos' p [] = ([],[])
parseBoardTijolos' p@(Pos x y) (c:cs) = case c of
    '?' -> (Pos x y : xs,ys)
    '#' -> (xs,Pos x y : ys)
    ' ' -> (xs,ys)
  where
    (xs,ys) = parseBoardTijolos' (Pos (succ x) y) cs

validaBoard :: Word8 -> Board -> P ()
validaBoard n tab = do
    let dim = boardDim tab
    unless (odd dim) $ unexpected "tabuleiro deve ter dimensão ímpar"
    --let allpos = [(Pos x y) | y::Word8 <- [1..n-2], x::Word8 <- [1..n-2] ]
    --let somepos = drop (fromEnum $ boardCaracol tab) allpos
    --let cantos = filter (isCorner n) somepos
    --forM_ cantos $ \c -> case lookPos tab c (closeParedes dim $ boardCaracol tab) of
    --    Just ' ' -> return ()
    --    x -> unexpected $ "posição " ++ show c ++ " deve ser espaço e não " ++ show x

parseLinhaMeio :: Word8 -> P String
parseLinhaMeio n = replicateM (fromEnum n) (parseCharMeio n)

parseCharMeio :: Word8 -> P Char
parseCharMeio n = do
    pos <- getPosition
    parsechar <- if isWall n (Pos (toEnum $ pred $ sourceLine pos) (toEnum $ pred $ sourceColumn pos)) []
        then return $ char '#'
        else return $ char ' ' <|> char '?' <|> char '#'
    parsechar

-- * Pretty printer

showpos (Pos x y) = show x ++ " " ++ show y
showpower p Bombs = "+ "++showpos p
showpower p Flames = "! "++showpos p
allBombsWithPlayer :: Mapa -> Map Pos (Word8,Bomb)
allBombsWithPlayer = Map.unions . map fuse . Map.toList . mBombs
  where fuse (i,xs) = Map.map (i,) xs
showbomb p (i,b) = "* "++showpos p++" "++show i++" "++show (bRadius b)++" "++show (bTime b)
showplayer p st = show p++" "++showpos (pPos st)++pws'
    where
    pws' = if List.null pws then "" else " " ++ pws
    pws = replicate (fromEnum $ pred $ pConstip st) '+'++replicate (fromEnum $ pred $ pRadius st) '!'

prettyMapa :: Mapa -> [String]
prettyMapa m = prettyBoard (board m) ++ pwBombs ++ pwFlames ++ mBombs ++ mPlayers
  where
  (bs,fs)  = Map.partition (==Bombs) (powers m)
  pwBombs  = Map.foldrWithKey (\p b xs -> showpower p b : xs) [] bs
  pwFlames = Map.foldrWithKey (\p b xs -> showpower p b : xs) [] fs
  mBombs   = Map.foldrWithKey (\p b xs -> showbomb p b : xs) [] (allBombsWithPlayer m)
  mPlayers = Map.foldrWithKey (\p b xs -> showplayer p b : xs) [] (players m)

prettyBoard :: Board -> [String]
prettyBoard board = split [genPos (Pos x y) | y::Word8 <- [0..dim-1], x::Word8 <- [0..dim-1]]
    where
    dim = fromIntegral $ boardDim board
    genPos :: Pos -> Char
    genPos p@(Pos x y) | isWall dim p closed = '#'
                   | List.elem p (boardTijolos board) = '?'
                   | otherwise = ' '
    split [] = []
    split xs | dim == 0 = []
    split xs = let (x,xs') = splitAt (fromEnum dim) xs in x : split xs'
    closed = closeParedes dim (boardCaracol board)

-- * Movimentacao

moveMapaMaybe :: MonadReader Mutation m => Int -> Word8 -> Maybe Char -> Mapa -> m Mapa
moveMapaMaybe bombTicks p Nothing m = return m
moveMapaMaybe bombTicks p (Just c) m = moveMapa bombTicks m p c

moveMapaPerfectBot :: Int -> Mapa -> Word8 -> Char -> (Mapa,[Word8])
moveMapaPerfectBot bombTicks m p c
    | List.elem c "UDLRB" = (moveMapaPerfect bombTicks m p c,[])
    | otherwise = let dead = if isJust (Map.lookup p $ players m) then [p] else []
                  in (m { players = Map.delete p $ players m },dead)

runPerfect :: Reader Mutation a -> a
runPerfect m = Reader.runReader m perfect

moveMapaPerfect :: Int -> Mapa -> Word8 -> Char -> Mapa
moveMapaPerfect bombTicks m p c = runPerfect (moveMapa bombTicks m p c) 

moveMapaPerfectMb :: Int -> Mapa -> Word8 -> Char -> Maybe Mapa
moveMapaPerfectMb moveMapa m p c = runPerfect (runMaybeT $ moveMapaMb moveMapa m p c)

up,down,left,right :: Pos -> Pos
up    (Pos x y) = Pos x (y-1)
down  (Pos x y) = Pos x (y+1)
left  (Pos x y) = Pos (x-1) y
right (Pos x y) = Pos (x+1) y

moveMapa :: MonadReader Mutation m => Int -> Mapa -> Word8 -> Char -> m Mapa
moveMapa bombTicks m p c = do
    mb <- runMaybeT (moveMapaMb bombTicks m p c)
    case mb of
        Just m' -> return m'
        Nothing -> return m

moveMapaMb :: MonadReader Mutation m => Int -> Mapa -> Word8 -> Char -> MaybeT m Mapa
moveMapaMb bombTicks m p c = do
    mutation <- Reader.ask
    let oridirs = [Pos (-1) 0,Pos 1 0,Pos 0 (-1),Pos 0 1]
    let dirs = case mutation of
                    Inverse -> reverse oridirs
                    otherwise -> oridirs
    if (mutation==DoesNotRecognize c)
        then return m
        else case c of
            'L' -> movePlayerMb m p $ dirs!!0
            'R' -> movePlayerMb m p $ dirs!!1
            'U' -> movePlayerMb m p $ dirs!!2
            'D' -> movePlayerMb m p $ dirs!!3
            'B' -> placeBombMb bombTicks m p

movePlayer :: MonadReader Mutation m => Mapa -> Word8 -> Pos -> MaybeT m Mapa
movePlayer m pl pos = do
    mb <- runMaybeT (movePlayerMb m pl pos)
    case mb of
        Just m' -> return m'
        Nothing -> return m

movePlayerMb :: MonadReader Mutation m => Mapa -> Word8 -> Pos -> MaybeT m Mapa
movePlayerMb m p (Pos mx my) = do
    mutation <- lift $ Reader.ask
    case mutation of
        IdentityProblems ((==p) -> True) -> fail "identity problems"
        otherwise -> modifyPlayer m p move
  where
  newPos (Pos x y) = Pos (x+mx) (y+my)
  move :: MonadReader Mutation m => PStat -> MaybeT m (PStat,Mapa -> MaybeT m Mapa)
  move pst = do
      let dim = boardDim $ board m
      ok <- checkPos m npos (closeParedes dim $ boardCaracol $ board m)
      if ok
        then do
            mutation <- Reader.ask
            let catchpw m = case mutation of
                            CantCatchPowerups -> return m
                            otherwise -> catchPowerUp m p npos
            return (pst { pPos = npos },catchpw)
        else fail "can't move player"
    where npos = newPos (pPos pst)

catchPowerUp :: MonadReader Mutation m => Mapa -> Word8 -> Pos -> m Mapa
catchPowerUp m p pos = modifyPowerUps m $ \powers -> case Map.lookup pos powers of
    Just pw -> do
        mutation <- Reader.ask
        let powers' = case mutation of
                        EternalPowerUps -> powers
                        otherwise -> Map.delete pos powers
        return (powers',\m -> modifyPlayer m p $ \pst -> liftM (,return) $ updPlayerPower pw pst)
    Nothing -> return (powers,return)

updPlayerPower :: MonadReader Mutation m => PowerUp -> PStat -> m PStat
updPlayerPower pw pst = do
    mutation <- Reader.ask
    case mutation of
        PowerlessPlayer -> return pst
        otherwise -> case pw of
            Bombs -> return $ pst { pConstip = succ $ pConstip pst }
            Flames -> return $ pst { pRadius = succ $ pRadius pst }

modifyMapa :: Monad m => Mapa -> (Mapa -> m (Mapa,Mapa -> m Mapa)) -> m Mapa
modifyMapa m f = do
    (m',rec) <- f m
    rec m'

modifyPowerUps :: Monad m => Mapa -> (PowerUps -> m (PowerUps,Mapa -> m Mapa)) -> m Mapa
modifyPowerUps m f = modifyMapa m $ \m -> do
    (powers',rec) <- f (powers m)
    return (m { powers = powers' },rec)

modifyPlayers :: Monad m => Mapa -> (Players -> m (Players,Mapa -> m Mapa)) -> m Mapa
modifyPlayers m f = modifyMapa m $ \m -> do
    (players',rec) <- f (players m)
    return (m { players = players' },rec)

modifyPlayer :: Monad m => Mapa -> Word8 -> (PStat -> m (PStat,Mapa -> m Mapa)) -> m Mapa
modifyPlayer m p f = modifyPlayers m $ \players -> case Map.lookup p players of
    Nothing -> return (players,return)
    Just pst -> do
        (pst',rec) <- f pst
        return (Map.insert p pst' players,rec)

modifyBombs :: Monad m => Mapa -> (Bombs -> m (Bombs,Mapa -> m Mapa)) -> m Mapa
modifyBombs m f = modifyMapa m $ \m -> do
    (bombs',rec) <- upd (Map.toList $ mBombs m)
    return (m { mBombs = Map.fromList bombs' },rec)
  where
    upd [] = return ([],return)
    upd ((x,y):xs) = do
        (y',rec) <- f y
        (xs',recs) <- upd xs
        return ((x,y'):xs',rec >=> recs)

modifyPBombs :: Monad m => Mapa -> Word8 -> (PStat -> Bombs -> m (Bombs,Mapa -> m Mapa)) -> m Mapa
modifyPBombs m p f = modifyMapa m $ \m -> do
    case Map.lookup p (players m) of
        Nothing -> return (m,return)
        Just pst -> do
            (bombs',rec) <- updPBombs p (f pst) (mBombs m)
            return (m { mBombs = bombs' },rec)

updPBombs :: Monad m => Word8 -> (Bombs -> m (Bombs,Mapa -> m Mapa)) -> Map Word8 Bombs -> m (Map Word8 Bombs,Mapa -> m Mapa)
updPBombs p f xs = case Map.lookup p xs of
    Nothing -> do
        (bs',rec) <- f Map.empty
        return (Map.insert p bs' xs,rec)
    Just bs -> do
        (bs',rec) <- f bs
        return (Map.insert p bs' xs,rec)

-- checks if a player can move to a position
checkPos :: MonadReader Mutation m => Mapa -> Pos -> [Pos] -> m Bool
checkPos m pos closed = do
    mutation <- Reader.ask
    let checkSpace = case lookPos (board m) pos closed of
                        Just ' ' -> return True
                        otherwise -> return False
    case mutation of
      MoveOverWallsOrBricks -> return True
      Perfect True -> if isJust (Map.lookup pos $ allBombs m) then return False else checkSpace
      InvisibleWall -> if Set.member pos (allBombPlayerPowerPositions m)
                        then return False
                        else checkSpace
      otherwise -> checkSpace
        
lookPos :: Board -> Pos -> [Pos] -> Maybe Char
lookPos tab p closed
    | isWall (fromIntegral $ boardDim tab) p closed = Just '#'
    | inRng (fromIntegral $ boardDim tab) p = Just $ if List.elem p (boardTijolos tab) then '?' else ' '
    | otherwise = Nothing

inRng :: Word8 -> Pos -> Bool
inRng d (Pos x y) = between 0 (d-1) x && between 0 (d-1) y

pBombs :: Word8 -> Mapa -> Bombs
pBombs p m = case Map.lookup p (mBombs m) of
    Nothing -> Map.empty
    Just bs -> bs

placeBomb :: MonadReader Mutation m => Int -> Mapa -> Word8 -> m Mapa
placeBomb bombTicks m p = do
    mb <- runMaybeT (placeBombMb bombTicks m p)
    case mb of
        Just m' -> return m'
        Nothing -> return m

placeBombMb :: MonadReader Mutation m => Int -> Mapa -> Word8 -> MaybeT m Mapa
placeBombMb bombTicks m p = do
    let allBombs = Map.unions $ Map.elems $ mBombs m
    mutation <- Reader.ask
    let canDrop pst = case mutation of
                        ObliviousPlayer -> 1 <= toEnum (Map.size (pBombs p m))
                        otherwise -> pConstip pst <= toEnum (Map.size (pBombs p m))
    let hasBomb pst = isJust (Map.lookup (pPos pst) allBombs)
    let cond pst = case mutation of
                NoMaxBombs -> hasBomb pst
                MultiBombs -> canDrop pst
                otherwise -> canDrop pst || hasBomb pst
    let newBomb pst = case mutation of
                        DefaultBombs -> Bomb 1 10
                        otherwise -> Bomb (pRadius pst) (toEnum bombTicks)
    modifyPBombs m p $ \pst bombs -> do
        if cond pst
            then fail "can't place bomb"
            else return (Map.insert (pPos pst) (newBomb pst) bombs,return)

-- * Tempo

-- (posição,código da imagem)
type Burned = [(Pos,Int)]

-- returns the number of closed blocks
closedBlocks :: Word8 -> Int -> Word16
closedBlocks dim ticksToEnd = toEnum $ if ticksToEnd > totalBlocks dim then 0 else totalBlocks dim - ticksToEnd

totalBlocks :: Word8 -> Int
totalBlocks dim = (fromEnum (dim-2))^2

closeParedes :: Word8 -> Word16 -> [Pos]
closeParedes dim n = take (fromEnum n) $ closeParedes' (dim-3) (Pos 1 1) 'R'
closeParedes' 1 p 'R' = [p] -- stop condition
closeParedes' dim (Pos x y) 'R' = map (flip Pos y) [x..x+dim-1]
    ++ closeParedes' dim (Pos (x+dim) y) 'D'
closeParedes' dim (Pos x y) 'D' = map (Pos x) [y..y+dim-1]
    ++ closeParedes' dim (Pos x (y+dim)) 'L'
closeParedes' dim (Pos x y) 'L' = reverse (map (flip Pos y) [x-dim+1..x])
    ++ closeParedes' dim (Pos (x-dim) y) 'U'
closeParedes' dim (Pos x y) 'U' = reverse (map (Pos x) [y-dim+1..y])
    ++ closeParedes' (caracol dim) (Pos (x+1) (y-dim+1)) 'R'

closeCaracol :: Mapa -> Int -> [Pos]
closeCaracol mapa ticksToEnd = closeParedes dim n
    where
    n = closedBlocks (boardDim $ board mapa) ticksToEnd
    dim = boardDim $ board mapa

caracol 0 = 1
caracol n | even n = n-2 

validaCaracol :: Word8 -> [Pos] -> P Word16
validaCaracol dim ps = checkCaracol ps caracol
    where
    caracol = closeParedes' (dim-3) (Pos 1 1) 'R'
    checkCaracol [] cs = return 0
    -- ambíguo: ao faze parsing do caracol não conseguimos saber qt tempo falta
    checkCaracol ps (c:cs) | List.elem c ps = liftM succ $ checkCaracol (List.delete c ps) cs
                           | isWall dim c [] = liftM succ $ checkCaracol (List.delete c ps) cs
    checkCaracol (p:ps) cs | isWall dim p [] = checkCaracol ps cs
    checkCaracol ps cs = unexpected $ "caracol inválido " ++ show ps

--normalizeTab :: String -> Maybe Int -> String
--normalizeTab tab Nothing = tab
--normalizeTab tab (Just ticks) = case leMapa tab of
--    Left err -> tab
--    Right mapa -> unlines0 $ prettyMapa mapa1
--        where
--        mapa1 = fst $ cleanseMap closedwalls mapa
--        closedwalls = closeCaracol mapa ticks

-- precisa de saber qts ticks faltam e em que posição está a fechar
-- returns dead players

avancaTempoMapa :: Mapa -> Int -> Mapa
avancaTempoMapa m t = foldl avancaTempoMapa' m [t..t-10]

avancaTempoMapa' :: Mapa -> Int -> Mapa
avancaTempoMapa' m t = fst $ fst $ avancaTempoPerfect (m,[]) t

avancaTempoPerfect :: (Mapa,Burned) -> Int -> ((Mapa,Burned),[Word8])
avancaTempoPerfect m t = runPerfect (avancaTempo m t)

avancaTempo :: MonadReader Mutation m => (Mapa,Burned) -> Int -> m ((Mapa,Burned),[Word8])
avancaTempo (mapa,burned) ticks = do mutation <- Reader.ask
                                     avancaTempoWalls (mapa,burned) (closedwalls mutation)
    where
    dim = boardDim $ board mapa
    -- calcula paredes fechadas
    nwalls = closedBlocks dim (ticks-1)
    closedwalls AFKSpiral = closeParedes dim 0
    closedwalls ErraticSpiral = closeParedes (dim+1) nwalls
    closedwalls _ = closeParedes dim nwalls

avancaTempoWalls :: MonadReader Mutation m => (Mapa,Burned) -> [Pos] -> m ((Mapa,Burned),[Word8])
avancaTempoWalls (mapa,burned) closedwalls = do
    mutation <- Reader.ask
    let dim = boardDim $ board mapa
    -- apaga bombas a zero e items queimados (jogadores,tijolos,powerups)
    (mapa1,deadplayers) <- cleanseMap (map fst burned) (closedwalls) mapa
    -- desce tempo das bombas e explode bombas a zero
    (mapa2,deadbombs) <- Writer.runWriterT $ modifyBombs mapa1 $ \bs -> do 
        let bs' = case mutation of 
                     PrematureBombs -> Map.map (\b -> b { bTime = 1 }) bs
                     StagnantBombs -> Map.map (\b -> b) bs
                     _ -> Map.map (\b -> b { bTime = pred $ bTime b }) bs
        let (dead,others) = Map.partition ((==0) . bTime) bs'
        Writer.tell dead
        return (others,return)
    (mapa3,(burned',deadplayers')) <- Writer.runWriterT $ explodeBombs False (Map.toList deadbombs) mapa2
    let deadplayers'' = case mutation of 
                          FakeFire -> []
                          _ -> deadplayers ++ deadplayers'
    let mapa4 = mapa3 { board = (board mapa3) { boardCaracol = toEnum $ length closedwalls } }
    return ((mapa4,burned'),deadplayers'')

explodeBurnedPerfect :: Bool -> [(Pos,Bomb)] -> Mapa -> Burned
explodeBurnedPerfect noSelf bs m = burned
  where
  (burned,dead) = Reader.runReader (Writer.execWriterT (explodeBombs noSelf bs m)) perfect

-- explode bombas e desenha chamas
explodeBombs :: MonadReader Mutation m => Bool -> [(Pos,Bomb)] -> Mapa -> WriterT (Burned,[Word8]) m Mapa
explodeBombs noSelf [] m = return m
explodeBombs noSelf (b:bs) m = explodeBomb noSelf b m >>= explodeBombs noSelf bs
explodeBomb :: MonadReader Mutation m => Bool -> (Pos,Bomb) -> Mapa -> WriterT (Burned,[Word8]) m Mapa
explodeBomb noSelf (p,b) m = do mutation <- Reader.ask 
                                let r = case mutation of
                                           UnderdevelopedBombs -> 1
                                           _ -> bRadius b
                                drawFlames noSelf p 0 Nothing r m

drawFlames :: MonadReader Mutation m => Bool -> Pos -> Word8 -> Maybe Char -> Word8 -> Mapa -> WriterT (Burned,[Word8]) m Mapa
drawFlames noSelf p@(Pos x y) 0 Nothing j m = do
    m1 <- if noSelf then return m else drawFlame p 0 m
    m2 <- drawFlames False (Pos x (y-1)) 1 (Just 'U') j m1
    m3 <- drawFlames False (Pos x (y+1)) 1 (Just 'D') j m2
    m4 <- drawFlames False (Pos (x-1) y) 1 (Just 'L') j m3
    drawFlames False (Pos (x+1) y) 1 (Just 'R') j m4
drawFlames noSelf p@(Pos x y) i (Just dir) j m = cont wall m $ case (i==j,dir) of
    (False,'U') -> drawFlame p 1 m >>= cont stop m . drawFlames False (Pos x (y-1)) (i+1) (Just dir) j
    (False,'D') -> drawFlame p 1 m >>= cont stop m . drawFlames False (Pos x (y+1)) (i+1) (Just dir) j
    (False,'L') -> drawFlame p 2 m >>= cont stop m . drawFlames False (Pos (x-1) y) (i+1) (Just dir) j
    (False,'R') -> drawFlame p 2 m >>= cont stop m . drawFlames False (Pos (x+1) y) (i+1) (Just dir) j
    (True,'U') -> drawFlame p 3 m
    (True,'D') -> drawFlame p 4 m
    (True,'L') -> drawFlame p 5 m
    (True,'R') -> drawFlame p 6 m
  where
    cont mb m f = mb >>= \b -> if b then return m else f
    bo = board m
    dim = boardDim bo
    wall = return $ isWall dim p $ closeParedes dim $ boardCaracol bo
    stop = isOpaque m p
drawFlame :: MonadReader Mutation m => Pos -> Int -> Mapa -> WriterT (Burned,[Word8]) m Mapa
drawFlame p i m = do
    mutation <- Reader.ask
    (m1,dead) <- case mutation of 
                     FakeFire -> return (m,[])
                     _ -> Writer.runWriterT (killPlayers [p] m)
    Writer.tell ([(p,i)],dead)
    case mutation of 
      FriendlyFire -> return m1
      _ -> modifyBombs m1 $ \bs -> do
        let bs' = Map.mapWithKey (\k b -> if p==k then b { bTime = 1 } else b) bs
        return (bs',return)

isBurned :: Burned -> Pos -> Bool
isBurned burned p = isJust $ List.lookup p burned

killPlayers :: MonadReader Mutation m => [Pos] -> Mapa -> WriterT [Word8] m Mapa
killPlayers allpos m = modifyPlayers m $ \ps -> do
    let (dead,ps') = Map.partition (\pst -> List.elem (pPos pst) allpos) ps
    Writer.tell $ Map.keys dead
    return (ps',return)

-- apaga bombas a zero e items queimados (jogadores,tijolos,powerups)
cleanseMap :: MonadReader Mutation m => [Pos] -> [Pos] -> Mapa -> m (Mapa,[Word8])
cleanseMap burned walls m = Writer.runWriterT $ do
    let allpos = burned++walls
    let tijs = boardTijolos $ board m
    -- apaga bombas a zero
    m1 <- modifyBombs m $ \bs -> do
        let (bs') = Map.filterWithKey (\p b -> bTime b /= 0 && not (List.elem p walls)) bs
        return (bs',return)
    -- apaga jogadores mortos
    mutation <- Reader.ask
    m2 <- case mutation of 
            FakeFire -> return m1
            _ -> killPlayers allpos m1
    -- apaga powerups destruídos
    m3 <- modifyPowerUps m2 $ \pws -> do
        let keepPowa k _ = not (List.elem k walls) && (not (List.elem k burned) || List.elem k tijs)
        let pws' = Map.filterWithKey keepPowa pws
        return (pws',return)
    -- apaga tijolos destruídos
    return $ m3 { board = f $ board m3 }
        where f b = b { boardTijolos = boardTijolos b \\ burned }

-- * Utilities

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

between :: Ord a => a -> a -> a -> Bool
between x y i = x <= i && i <= y

swap (x,y) = (y,x)

fromPos :: Pos -> (Int,Int)
fromPos (Pos x y) = (fromEnum x,fromEnum y)

-- * Tarefas

tarefa2 :: [String] -> Word8 -> Char -> [String]
tarefa2 tab p c = case leMapa (unlines tab) of
    Left err -> error $ show err
    Right m -> prettyMapa $ Reader.runReader (moveMapa 10 m p c) perfect

tarefa4IntervalPerfect :: Mapa -> Int -> Int -> Mapa
tarefa4IntervalPerfect m start end = Reader.runReader (tarefa4Interval m start end) perfect

tarefa4Interval :: MonadReader Mutation m => Mapa -> Int -> Int -> m Mapa
tarefa4Interval m start end
    | start < end = return m
    | otherwise = do
    m' <- tarefa4 m start
    tarefa4Interval m' (pred start) end

tarefa4Perfect :: Mapa -> Int -> Mapa
tarefa4Perfect m ticks = Reader.runReader (tarefa4 m ticks) perfect

tarefa4 :: MonadReader Mutation m => Mapa -> Int -> m Mapa
tarefa4 m ticks = do
  ((m1,burned1),_) <- avancaTempo (m,[]) ticks
  (m2,deads) <- cleanseMap (map fst burned1) [] m1
  mutation <- Reader.ask
  let m3 = case mutation of 
              FakeFire -> m2 
              _ -> m2 { players = Map.filterWithKey (\k v -> not $ List.elem k deads) $ players m2 }
  return m3

tarefa4WallsPerfect :: Mapa -> [Pos] -> Mapa
tarefa4WallsPerfect m closedwalls = Reader.runReader (tarefa4Walls m closedwalls) perfect
  
tarefa4Walls :: MonadReader Mutation m => Mapa -> [Pos] -> m Mapa
tarefa4Walls m closedwalls = do
  ((m1,burned1),_) <- avancaTempoWalls (m,[]) closedwalls
  (m2,deads) <- cleanseMap (map fst burned1) [] m1
  mutation <- Reader.ask
  let m3 = case mutation of 
              FakeFire -> m2 
              _ -> m2 { players = Map.filterWithKey (\k v -> not $ List.elem k deads) $ players m2 }
  return m3

-- * Mutações

data Mutation
    = Perfect Bool --bombas bloqueiam ou não
    | Inverse -- teclas trocadas
    | DoesNotRecognize Char -- não efectua a acção
    | NoMaxBombs -- não verifica condição limite de bombas
    | MoveOverWallsOrBricks -- vai pra cima de paredes e tijolos
    | DefaultBombs -- põe bombas sem ter em conta os stats do jogador
    | CantCatchPowerups -- jogador não apanha powerups
    | EternalPowerUps -- powerup não desaparece qd apanhado
    | PowerlessPlayer -- stats do jogador não mudam qd apanha powerups
    | MultiBombs -- consegue pôr bombas em cima de outras
    | InvisibleWall -- jogador não se mexe para cima de bombas/powerups/outros jogadores
    | IdentityProblems Word8 -- não mexe o jogador esperado
    | ObliviousPlayer -- não põe mais bombas mesmo tendo powerups
    | AFKSpiral -- não larga pedras
    | ErraticSpiral -- larga pedras numa posição subjacente
    | StagnantBombs -- bombas não explodem
    | PrematureBombs -- bombas esplodem antes do tempo
    | UnderdevelopedBombs -- bombas esplodem sempre com a mensa potência
    | FriendlyFire -- bombas não fazem reações em cadeia
    | FakeFire -- bombas não matam jogadores
  deriving (Eq,Show)


mutations1F :: [Mutation]
mutations1F = [Inverse,DoesNotRecognize 'L',DoesNotRecognize 'R',DoesNotRecognize 'U',DoesNotRecognize 'D',DoesNotRecognize 'B'
              ,NoMaxBombs,MoveOverWallsOrBricks,DefaultBombs,CantCatchPowerups,EternalPowerUps,PowerlessPlayer,MultiBombs
              ,InvisibleWall,ObliviousPlayer,IdentityProblems 0,IdentityProblems 1,IdentityProblems 2, IdentityProblems 3]

mutations2F :: [Mutation]
mutations2F = [AFKSpiral, ErraticSpiral, PrematureBombs, UnderdevelopedBombs, FriendlyFire, FakeFire]

perfect = Perfect False


