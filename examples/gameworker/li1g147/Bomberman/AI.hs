{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Bomberman.AI where

import Bomberman.Engine
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List as List
import Data.Word
import Data.Maybe
import Safe

import Control.Monad.Identity
import Control.Monad.Writer (Writer(..))
import qualified Control.Monad.Writer as Writer

--import Debug.Trace

--import Utils

type Play = Maybe Char
allPlays :: [Play]
allPlays = [Nothing,Just 'U',Just 'D',Just 'L',Just 'R',Just 'B']

-- the best center position
centerPos :: Word8 -> Pos
centerPos dim = if even (cols dim) then Pos c c else Pos (c-1) c
    where
    c = div (dim-1) 2
    cols :: Word8 -> Word8
    cols 1 = 1
    cols n = succ (cols (n-2))

-- arithmetic distance
distance :: Pos -> Pos -> Float
distance (Pos x1 y1) (Pos x2 y2) = average2 (dist x1 x2) (dist y1 y2)
    where
    dist x y = realToFrac (if x > y then x-y else y-x)

-- a tree of moves
data PlayTree = PlayTree (Map Play (Score,PlayTree))
    deriving (Eq,Show)

moveMapaPerfectMbPlay :: Mapa -> Word8 -> Maybe Char -> Maybe Mapa
moveMapaPerfectMbPlay m p Nothing = Just m -- not moving is a valid option
moveMapaPerfectMbPlay m p (Just c) = moveMapaPerfectMb 3 m p c

computePlays :: Mapa -> Int -> Word8 -> PlayTree
computePlays mapa ticks player = computePlays' 1 mapa caracol player
    where
    dim = boardDim $ board mapa
    center = centerPos dim
    -- caracol futuro, fecha 1 parede por tick
    caracol = if ticks > totalBlocks dim
        then replicate (ticks - totalBlocks dim) Nothing ++ (map Just $ closeCaracol mapa 0)
        else drop (totalBlocks dim - ticks) (map Just $ closeCaracol mapa 0)
    computePlays' it mapa (c:cs) player = PlayTree $ Map.fromList $ catMaybes $ map move allPlays
        where
        move :: Play -> Maybe (Play,(Score,PlayTree))
        move play = case moveMapaPerfectMbPlay mapa player play of
            Nothing -> Nothing -- ignore invalid moves
            Just mapa' -> let mapa'' = tarefa4Walls mapa' (maybeToList c)
                              ps = players mapa''
                          in case Map.lookup player ps of
                              Nothing -> Nothing -- ignore dying scenarios
                              Just pst -> let dist = distance center (pPos pst)
                                              ops = pred $ Map.size ps
                                              bricks = length $ boardTijolos $ board mapa''
                                              chains = case play of { Just 'B' -> computeChain mapa''; otherwise -> 0 }
                                              powas = fromEnum (pConstip pst) + fromEnum (pRadius pst)
                                              score = Score (ops,it) (powas,it) (chains,it) (bricks,it) (dist,it)
                                          in Just (play,(score,computePlays' (succ it) mapa'' cs player))

computeChain :: Mapa -> Int
computeChain m = length $ List.intersect (map fst burned) (map fst bs)
    where
    bs = Map.toList $ allBombs m
    burned = Writer.execWriter $ explodeBombs True bs m

prunePlays :: Int -> PlayTree -> PlayTree
prunePlays n t = maybe (PlayTree Map.empty) id (prunePlays' n t)
    where
    -- eliminate branches with a certain death (i.e, those with empty maps)
    prunePlays' :: Int -> PlayTree -> Maybe PlayTree
    prunePlays' 0 t = Just $ PlayTree Map.empty
    prunePlays' n (PlayTree xs) = if Map.null xs' then Nothing else Just $ PlayTree xs'
        where
        xs' = Map.foldlWithKey' acc Map.empty xs
        acc xs p (s,t) = case prunePlays' (pred n) t of
            Nothing -> xs
            Just t' -> Map.insert p (s,t') xs

data Score = Score { sOponents :: (Int,Int), sPowers :: (Int,Int), sChain :: (Int,Int), sBricks :: (Int,Int), sDistCenter :: (Float,Int) }
    deriving (Eq,Show)
instance Monoid Score where
    mempty = Score (3,1000) (0,1000) (0,1000) (1000,1000) (1000,1000)
    mappend s1@(Score x1 x2 x3 x4 x5) s2@(Score y1 y2 y3 y4 y5) = Score (min x1 y1) (maxmin x2 y2) (maxmin x3 y3) (min x4 y4) (min x5 y5)
instance Ord Score where
    compare s1@(Score x1 x2 x3 x4 x5) s2@(Score y1 y2 y3 y4 y5) = mconcat [compare x1 y1,comparemaxmin x2 y2,comparemaxmin x3 y3,compare x4 y4,compare x5 y5]

comparemaxmin :: (Ord a,Ord b) => (a,b) -> (a,b) -> Ordering
comparemaxmin (x1,x2) (y1,y2) = mconcat [compare y1 x1,compare x2 y2]

maxmin :: (Ord a,Ord b) => (a,b) -> (a,b) -> (a,b)
maxmin (x1,x2) (y1,y2)
    | x1 == y1 = (x1,min x2 y2)
    | otherwise = max (x1,x2) (y1,y2)

scorePlays :: PlayTree -> Map Play Score
scorePlays (PlayTree xs) = Map.map scorePlay xs
    where
    scorePlay :: (Score,PlayTree) -> Score
    scorePlay (score,t) = mappend score $ mconcat $ Map.elems scores
        where
        scores = scorePlays t

botString :: [String] -> Int -> Int -> Maybe Char
botString mapa player ticks = case leMapa (unlines mapa) of
    Left err -> error "bot: mapa inválido"
    Right m -> bot m player ticks

bot :: Mapa -> Int -> Int -> Maybe Char
bot mapa player ticks = case Map.toList scores of
    [] -> {-trace ("random") $ -} allPlays !! (ticks `mod` 6) -- sort of random
    xs -> let bscores = sortBy best xs
          in {-trace (show player ++ ": " ++ show bscores) $ -} fst $ head bscores
  where
    msg = show player ++ ": " ++ show scores
    best (x1,s1) (x2,s2) = mconcat [compare s1 s2,compareMove x1 x2]
    plays = prunePlays 5 $ computePlays mapa (pred ticks) (toEnum player)
    scores = scorePlays plays

compareMove :: Maybe Char -> Maybe Char -> Ordering
compareMove x y | x == y = EQ
compareMove x@(Just 'B') y = LT
compareMove x y@(Just 'B') = GT
compareMove Nothing y = GT
compareMove x Nothing = LT
compareMove x y = compare x y


average2 :: Fractional a => a -> a -> a
average2 x y = (x + y) / 2
