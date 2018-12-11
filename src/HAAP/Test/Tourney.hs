{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Tourney@ plugin to run tournaments.
-}

{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TupleSections, TypeOperators, DeriveFunctor, DeriveAnyClass, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, EmptyDataDecls, TemplateHaskell, DeriveGeneric, ViewPatterns, ScopedTypeVariables, RankNTypes #-}
module HAAP.Test.Tourney where

import HAAP.Core
import HAAP.IO
import HAAP.Pretty
import HAAP.DB
import HAAP.DB.State
import HAAP.Utils
import HAAP.Web.Hakyll
import HAAP.Web.Blaze
import HAAP.Log
import HAAP.Plugin
import HAAP.Lens

import Data.Maybe
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Time.LocalTime
import Data.Binary
import Data.Typeable
import Data.Data
import Data.Acid
import Data.SafeCopy
import Data.Default
import qualified Data.Text as T

--import Control.Monad.Except
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State as State
import Control.Monad.Reader as Reader
import Control.Monad.Morph
import Control.Monad.Trans.Compose
import Control.DeepSeq
--import Control.Monad.Catch
import Control.Exception.Safe

import System.Random.Shuffle
import System.Directory
import System.FilePath

import GHC.Generics (Generic(..))
import GHC.Stack

import qualified Text.Blaze.Html5 as H

data Tourney
data TourneyArgs = TourneyArgs
    { 
    }

instance Default TourneyArgs where
    def = TourneyArgs {}

instance HaapPlugin Tourney where
    type PluginI Tourney = TourneyArgs
    type PluginT Tourney = ReaderT TourneyArgs
    type PluginO Tourney = ()
    type PluginK Tourney t m = ()
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . getComposeT) m
        return (x,())

useTourney :: (HaapStack t m,PluginK Tourney t m) => Haap (PluginT Tourney :..: t) m a -> Haap t m a
useTourney m = usePlugin_ (return def) m

instance HaapMonad m => HasPlugin Tourney (ReaderT TourneyArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin Tourney (ComposeT (ReaderT TourneyArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m

-- | Player names need to be unique
class (Ord a,Pretty a) => TourneyPlayer a where
    defaultPlayer :: IO a -- players should be unique
    isDefaultPlayer :: a -> Bool
    renderPlayer :: a -> Html
    renderPlayer p = H.preEscapedToMarkup (prettyText p)

data HaapTourney t m db a r = HaapTourney
    { tourneyMax :: Int -- maximum number of table entries for the tournament
    , tourneyTitle :: T.Text
    , tourneyNotes :: T.Text
    , tourneyRounds :: Int -> [TourneyRound] -- determine the number of rounds 
    , tourneyPlayerTag :: T.Text
    , tourneyPlayers :: Either [a] [[a]] -- must be unique (left = unpaired players, right = paired players)
    , tourneyPath :: FilePath -- web folder where to render the tournaments
    , lensTourneyDB :: DBLens db (HaapTourneyDB a)
    , tourneyMatch :: HasDB db t m => Int -- tourneyno
                   -> Int -- roundno
                   -> Int -- matchno
                   -> [a] -- players
                   -> Haap t m ([(a,Int)],r) -- returning a match result
    , renderMatch :: r -> Rules Link -- renders a match result as a link
    , deleteTourney :: HasDB db t m => Int -- tourneyno
                    -> Haap t m () -- cleanup procedure
    }

data TourneyRound = TourneyRound
    { tourneyRoundSize :: Int -- number of players per round
    , tourneyRoundMatchSize :: Int -- number of players per match
    , tourneyRoundMatchWinners :: Int -- number of winning players
    , tourneyRoundBestOf :: Int -- number of matches per round (to the best of n wins)
    }

type Link = FilePath

emptyHaapTourneyDB = HaapTourneyDB 1 []

data HaapTourneyDB a = HaapTourneyDB
    { tourneyNo :: Int
    , tourneyDB :: [(Int,HaapTourneySt a)]
    }
  deriving (Show,Eq,Ord,Data,Generic,Typeable)
instance Binary a => Binary (HaapTourneyDB a)

insertHaapTourneySt :: HasDB db t m => HaapTourney t m db a r -> Int -> HaapTourneySt a -> HaapTourneyDB a -> Haap t m (HaapTourneyDB a)
insertHaapTourneySt t i tour st = do
    let tours = tourneyDB st
    tours' <- if length tours >= tourneyMax t
        then do
            deleteTourney t $ fst $ head tours
            return $ tail tours ++ [(i,tour)]
        else return $ tourneyDB st ++ [(i,tour)]
    return $ st { tourneyNo = succ $ tourneyNo st, tourneyDB = tours' }

-- map from player to round positions
type HaapTourneySt a = Map a Float
type HaapTourneySt' a = Map a [PlayerPos] -- list because player names do not need to be unique

type PlayerPos = [Int] -- sequence of positions

type TourneyTree a r = [Round a r]
type Round a r = [[Match a r]]
type Match a r = ([((a,Int),Bool)],[r])

--getTourneySize :: (HasCallStack,HasDB db t m) => Proxy db -> [a] -> Haap t m Int
--getTourneySize _ (length -> n)
--    | n <= 4 = return 4
--    | n <= 16 = return 16
--    | n <= 32 = return 32
--    | n <= 64 = return 64
--    | n <= 128 = return 128
--    | n <= 256 = return 256
--    | otherwise = throw $ HaapException callStack $ "unsupported tourney size " <> prettyText n
--
--roundWinners :: Int -> Int
--roundWinners 256 = 1
--roundWinners 128 = 2
--roundWinners 64 = 1
--roundWinners 32 = 2
--roundWinners 16 = 1
--roundWinners 4 = 1
--roundWinners n = error $ "unsupported roundWinners " ++ show n
--
--roundLosers :: Int -> [Int]
--roundLosers 256 = [65,66,67]
--roundLosers 128 = [33,34]
--roundLosers 64 = [17,18,19]
--roundLosers 32 = [17,18]
--roundLosers 16 = [5,6,7]
--roundLosers 4 = [2,3,4]
--roundLosers n = error $ "unsupported roundLosers " ++ show n
--
--nextRound :: Int -> Int
--nextRound 256 = 64
--nextRound 128 = 64
--nextRound 64 = 16
--nextRound 32 = 16
--nextRound 16 = 4
--nextRound 4 = 1
--nextRound n = error $ "nextRound " ++ show n

averagePlayerPos :: [TourneyRound] -> [PlayerPos] -> Float
averagePlayerPos rounds [] = 0
averagePlayerPos rounds xs = averageList $ map (realToFrac . playerPos rounds) xs

playerPos :: [TourneyRound] -> [Int] -> Int
playerPos (round:rounds) [] = tourneyRoundSize round
playerPos [round] [x] = x
playerPos (round:rounds) [x] = tourneyRoundSize (head rounds) + 1
playerPos (round:rounds) (x:xs) = playerPos rounds xs

runHaapTourney :: (Show a,HasPlugin Tourney t m,PluginK db t m,MonadIO m,NFData a,TourneyPlayer a,HasDB db t m) => HaapTourney t m db a r -> Haap t m (Int,HaapTourneyDB a,TourneyTree a r,ZonedTime)
runHaapTourney (tourney::HaapTourney t m db a r) = do
    --logEvent "haaptourney"
    let players = tourneyPlayers tourney
    --logEvent "haaptourneysize"
    nplayers <- case players of
        Left ps -> return $ length ps
        Right ps -> return $ length $ concat ps
    let rounds = tourneyRounds tourney nplayers
    --tourneySize <- case players of
    --    Left ps -> case rounds of
    --        [] -> return $ length ps
    --        otherwise -> return $ tourneyRoundSize $ head rounds
    --    Right ps -> return $ length $ concat ps
    --logEvent "haaptourneypair"
    matches <- pairPlayers (Proxy::Proxy db) players (head rounds)
    --logEvent "haaptourneyquery"
    db <- queryDB $ dbGet $ lensTourneyDB tourney
    let tourneyno = tourneyNo db
    --logEvent "playtourney"
    (tourneyst,tree) <- playTourney tourney matches tourneyno rounds
--    runIO $ putStrLn $ pretty $ sort $ Map.toAscList tourneyst
    let tourneyst' = Map.map (averagePlayerPos rounds) tourneyst
--    runIO $ putStrLn $ pretty $ sort $ Map.toAscList tourneyst'
    db' <- insertHaapTourneySt tourney tourneyno tourneyst' db
    updateDB $ dbPut (lensTourneyDB tourney) db'
    tourneytime <- runBaseIO' getZonedTime
    return (tourneyno,db',tree,tourneytime)

-- shuffles and splits players into groups of 4
pairPlayers :: (HasCallStack,Show a,MonadIO m,NFData a,TourneyPlayer a,HasDB db t m) => Proxy db -> Either [a] [[a]] -> TourneyRound -> Haap t m [[a]]
pairPlayers _ (Right players) round_ = return players
pairPlayers _ (Left players) round_ = do
    logEvent $ "Pairing for tourney size " <> prettyText tourneySize
    players' <- runBaseIO' $ shuffleM players
    let (randoms,nonrandoms) = partition isDefaultPlayer players'
    bots <- runBaseIO' $ replicateM (tourneySize-length players') defaultPlayer
    logEvent $ "Pairing " <> prettyText (length nonrandoms) <> " + " <> prettyText (length randoms) <> " players with " <> prettyText (length bots) <> " bots"
    let xxs = pair (tourneySize `div` matchSize) nonrandoms (randoms++bots) 
    if validaMatches xxs
        then return xxs
        else do
            let stack = callStack
            throw $ HaapException stack $ "pairPlayers: " <> prettyText (length xxs) <> " " <> prettyText (show xxs)
  where
    tourneySize = tourneyRoundSize round_
    matchSize = tourneyRoundMatchSize round_
    validaMatches xs = all ((==matchSize) . length) xs && length xs == (tourneySize `div` matchSize)
    
    pair :: Int -> [a] -> [a] -> [[a]]
    pair potsize xs ys = toMatrix potsize (ys ++ xs)
    toMatrix :: Int -> [a] -> [[a]]
    toMatrix potsize [] = replicate potsize []
    toMatrix potsize xs = map (uncurry (:)) $ zip xs1 (toMatrix potsize xs2)
        where (xs1,xs2) = splitAt potsize xs
    

-- (tourney,tourney no, round no,match no,partial rankings)
type PlaySt t m db a r = (HaapTourney t m db a r,Int,Int,Int,HaapTourneySt' a)

type HaapPlay t m db a r = StateT (PlaySt t m db a r) (Haap t m)

playTourney :: (MonadIO m,HasDB db t m,TourneyPlayer a) => HaapTourney t m db a r -> [[a]] -> Int -> [TourneyRound] -> Haap t m (HaapTourneySt' a,TourneyTree a r)
playTourney tourney matches tourneyno rounds = do
    (tree,(_,_,_,_,rank)) <- State.runStateT
        (playRounds matches rounds)
        (tourney,tourneyno,1,1,Map.empty)
    return (rank,tree)

playRounds :: (MonadIO m,HasDB db t m,TourneyPlayer a) => [[a]] -> [TourneyRound] -> HaapPlay t m db a r [Round a r]
playRounds matches [] = return []
playRounds matches (round_:rounds_) = do
    (winners,roundRes) <- playRound matches round_
    if null rounds_
        then return [[roundRes]]
        else do
            let matchSize = tourneyRoundMatchSize round_
            let matchWinners = tourneyRoundMatchWinners round_
            let nextMatchSize = tourneyRoundMatchSize $ head rounds_
            roundRess <- playRounds (groupN nextMatchSize $ map fst winners) rounds_
            return (groupN (divNote "playRounds" matchSize matchWinners) roundRes:roundRess)

-- returns (standings for players that lost this round, winner players for next round)
playRound :: (MonadIO m,HasDB db t m,TourneyPlayer a) => [[a]] -> TourneyRound -> HaapPlay t m db a r ([(a,Int)],[Match a r])
playRound matches round_ = do
    (_,_,rno,_,_) <- State.get
    lift $ logEvent $ "playing round " <> prettyText rno
    State.modify $ \(o,x,y,z,w) -> (o,x,y,1,w)
    (matches',roundRes) <- playMatches matches round_
    let uniquematches' = Map.toList $ foldr (\(a,x) m -> Map.insertWith (++) a [x] m) Map.empty (concat matches')    
--    lift $ runIO $ putStrLn $ "matches " ++ pretty (sort matches')
--    lift $ runIO $ putStrLn $ "umatches " ++ pretty (sort uniquematches')
    let winners = concatMap (take (tourneyRoundMatchWinners round_)) matches'
    State.modify $ \(o,x,y,z,w) -> (o,x,y+1,z,addPlayerSts uniquematches' w)
    (_,_,_,_,newst) <- State.get
--    lift $ runIO $ putStrLn $ "newst " ++ pretty (sort $ Map.toAscList newst)
    return (winners,roundRes)

addPlayerSt :: (TourneyPlayer a) => (a,[Int]) -> HaapTourneySt' a -> HaapTourneySt' a
addPlayerSt (p,r) xs = Map.insertWith aux p (map (:[]) r) xs
    where
    aux :: [PlayerPos] -> [PlayerPos] -> [PlayerPos]
    aux xs x = map (\(ys,y) -> maybe ys (++ys) y) (zipLeft xs x)

addPlayerSts :: TourneyPlayer a => [(a,[Int])] -> HaapTourneySt' a -> HaapTourneySt' a
addPlayerSts [] m = m
addPlayerSts (x:xs) m = addPlayerSt x (addPlayerSts xs m)

playMatches :: (MonadIO m,HasDB db t m) => TourneyPlayer a => [[a]] -> TourneyRound -> HaapPlay t m db a r ([[(a,Int)]],[Match a r])
playMatches xs round_ = do
    lift $ logEvent $ "playing matches "
    --(tourney,tourneyno,roundno,matchno,_) <- State.get
    let bestof = tourneyRoundBestOf round_
    scores <- forM xs (playMatchBest bestof)
    return (map fst scores,map (mapFst addWinner) scores)
  where
    nwinners = tourneyRoundMatchWinners round_
    addWinner :: [(a,Int)] -> [((a,Int),Bool)]
    addWinner xs = zip xs (replicate nwinners True ++ repeat False)

playMatchBest :: (MonadIO m,HasDB db t m,TourneyPlayer a) => Int -> [a] -> HaapPlay t m db a r ([(a,Int)],[r])
playMatchBest bestof xs = playMatchBest' [] (Map.fromList $ map (\x -> (x,[])) xs)
    where
    intToFloat :: Int -> Float
    intToFloat = realToFrac
    isWinner xs = length (filter (==1) xs) >= bestof
    playMatchBest' :: (MonadIO m,HasDB db t m,TourneyPlayer a) => [r] -> Map (a) [Int] -> HaapPlay t m db a r ([(a,Int)],[r])
    playMatchBest' replays players = do
        lift $ logEvent $ "playMatchBest " <> prettyText bestof <> prettyText (players)
        let winners = Map.filter isWinner players
        if Map.null winners
            then do
                (res,replay) <- playMatch (Map.keys players)
                let replays' = replays++[replay]
                let players' = Map.unionWith (++) players (Map.map (\x -> [x]) $ Map.fromList res)
                playMatchBest' replays' players'
            else do
                let rank = rankGroups players
                lift $ logEvent $ "match rank" <> prettyText rank
                return (rank,replays)

rankGroups :: Ord a => Map.Map a [Int] -> [(a,Int)]
rankGroups ms = rank 0 Nothing $ sortBy cmpsnd $ Map.toList (Map.map (map (neg . length) . mygroup . sort) ms)
    where
    neg x = (-x)
    cmpsnd x y = compare (snd x) (snd y)
    rank p _ [] = []
    rank p (Just j) ((pl,i):xs) | i == j = (pl,p) : rank (p) (Just j) xs
                                | otherwise = (pl,succ p) : rank (succ p) (Just i) xs
    rank p Nothing ((pl,i):xs) = (pl,succ p) : rank (succ p) (Just i) xs
    mygroup :: [Int] -> [[Int]]
    mygroup xs = mygroupN 1 (Map.size ms) xs
    mygroupN :: Int -> Int -> [Int] -> [[Int]]
    mygroupN i j xs | i > j = []
    mygroupN i j xs = let (ys,zs) = partition (==i) xs in ys : mygroupN (succ i) j zs

playMatch :: (MonadIO m,HasDB db t m,TourneyPlayer a) => [a] -> HaapPlay t m db a r ([(a,Int)],r)
playMatch xs = do
    (tourney,tourneyno,roundno,matchno,_) <- State.get
    lift $ logEvent $ "playing match " <> prettyText roundno <> " " <> prettyText matchno
    r <- lift $ tourneyMatch tourney tourneyno roundno matchno xs
    State.modify $ \(tourney,tourneyno,roundno,matchno,w) -> (tourney,tourneyno,roundno,matchno+1,w)
    return $ mapFst (sortBy cmpsnd) r
  where
    cmpsnd x y = compare (snd x) (snd y)
    

instance (Ord a,SafeCopy a) => SafeCopy (HaapTourneyDB a) where
    putCopy (HaapTourneyDB no db) = contain $ do
        safePut no
        safePut db
    getCopy = contain $ do
        no <- safeGet
        db <- safeGet
        return $ HaapTourneyDB no db


