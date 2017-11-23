{-# LANGUAGE TemplateHaskell, DeriveGeneric, ViewPatterns, ScopedTypeVariables, RankNTypes #-}
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

import Data.Maybe
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Time.LocalTime
import Data.Binary
import Data.Typeable
import Data.Acid
import Data.SafeCopy

import Control.Monad.Except
import Control.Monad.State (StateT(..))
import qualified Control.Monad.State as State
import Control.DeepSeq

import System.Random.Shuffle
import System.Directory
import System.FilePath

import GHC.Generics (Generic(..))

import qualified Text.Blaze.Html5 as H

class (Ord a,Out a) => TourneyPlayer a where
    defaultPlayer :: a
    isDefaultPlayer :: a -> Bool
    renderPlayer :: a -> Html
    renderPlayer p = H.preEscapedToMarkup (pretty p)

data HaapTourney p args db m a r = HaapTourney
    { tourneyMax :: Int -- maximum number of table entries for the tournament
    , tourneyTitle :: String
    , tourneyPlayerTag :: String
    , tourneyPlayers :: [a]
    , tourneyPath :: FilePath -- web folder where to render the tournaments
    , lensTourneyDB :: DBLens db (HaapTourneyDB a)
    , tourneyMatch :: Int -- tourneyno
                   -> Int -- roundno
                   -> Int -- matchno
                   -> [a] -- players
                   -> Haap p args (DB db) m ([(a,Int)],r) -- returning a match result
    , renderMatch :: r -> Rules [Link] -- renders a match result as a series of links
    , deleteTourney :: Int -- tourneyno
                    -> Haap p args (DB db) m () -- cleanup procedure
    }

type Link = FilePath

emptyHaapTourneyDB = HaapTourneyDB 1 []

data HaapTourneyDB a = HaapTourneyDB
    { tourneyNo :: Int
    , tourneyDB :: [(Int,HaapTourneySt a)]
    }
  deriving (Generic,Typeable)
instance Binary a => Binary (HaapTourneyDB a)

insertHaapTourneySt :: HaapMonad m => HaapTourney p args db m a r -> Int -> HaapTourneySt a -> HaapTourneyDB a -> Haap p args (DB db) m (HaapTourneyDB a)
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
type Round a r = [Match a r]
type Match a r = ([a],r)

-- TODO: Tourney matches are currently fixed to multiples of 4 players
getTourneySize :: HaapMonad m => [a] -> Haap p args db m Int
getTourneySize (length -> n)
    | n <= 128 = return 128
    | n <= 256 = return 256
    | otherwise = throwError $ HaapException $ "unsupported tourney size " ++ show n

tourneyDiv :: Int -> Int
tourneyDiv 256 = 64
tourneyDiv 128 = 32
tourneyDiv 64 = 16
tourneyDiv 16 = 4
tourneyDiv 4 = 1

roundWinners :: Int -> Int
roundWinners 256 = 1
roundWinners 128 = 2
roundWinners 64 = 1
roundWinners 16 = 1
roundWinners 4 = 1
roundWinners n = error $ "unsupported roundWinners " ++ show n

roundLosers :: Int -> [Int]
roundLosers 256 = [65,66,67]
roundLosers 128 = [33,65]
roundLosers 64 = [17,18,19]
roundLosers 16 = [5,6,7]
roundLosers 4 = [2,3,4]
roundLosers n = error $ "unsupported roundLosers " ++ show n

nextRound :: Int -> Int
nextRound 256 = 64
nextRound 128 = 64
nextRound 64 = 16
nextRound 16 = 4
nextRound 4 = 1
nextRound n = error $ "nextRound " ++ show n

averagePlayerPos :: Int -> [PlayerPos] -> Float
averagePlayerPos no [] = 0
averagePlayerPos no xs = averageList $ map (realToFrac . playerPos no) xs

playerPos :: Int -> [Int] -> Int
playerPos roundno [] = roundno
playerPos 4 [x] = x
playerPos 1 xs = error $ "playerPos1 " ++ show xs
playerPos roundno [x] = nextRound roundno + 1
playerPos roundno (x:xs) = playerPos (nextRound roundno) xs

runHaapTourney :: (NFData a,HaapMonad m,HaapDB db,TourneyPlayer a) => HaapTourney p args db m a r -> Haap p args (DB db) m (Int,HaapTourneyDB a,TourneyTree a r,ZonedTime)
runHaapTourney tourney = do
    let players = tourneyPlayers tourney
    tourneySize <- getTourneySize players
    matches <- pairPlayers players tourneySize
    db <- queryDB $ dbGet $ lensTourneyDB tourney
    let tourneyno = tourneyNo db
    (tourneyst,tree) <- playTourney tourney matches tourneyno tourneySize
--    runIO $ putStrLn $ pretty $ sort $ Map.toAscList tourneyst
    let tourneyst' = Map.map (averagePlayerPos tourneySize) tourneyst
--    runIO $ putStrLn $ pretty $ sort $ Map.toAscList tourneyst'
    db' <- insertHaapTourneySt tourney tourneyno tourneyst' db
    updateDB $ dbPut (lensTourneyDB tourney) db'
    tourneytime <- runIO' getZonedTime
    return (tourneyno,db',tree,tourneytime)

-- shuffles and splits players into groups of 4
pairPlayers :: (NFData a,HaapMonad m,TourneyPlayer a) => [a] -> Int -> Haap p args db m [[a]]
pairPlayers players tourneySize = do
    players' <- runIO' $ shuffleM players
    let (randoms,nonrandoms) = partition isDefaultPlayer players'
    let bots = replicate (tourneySize-length players') defaultPlayer
    let by = fromIntegral (length bots + length randoms) / fromIntegral (tourneyDiv tourneySize)
    let xxs = pair by 0 nonrandoms (randoms++bots)
    if validaMatches xxs
        then return xxs
        else throwError $ HaapException $ "pairPlayers: "  ++ pretty xxs ++ "\n" ++ show (length xxs)
  where
    validaMatches xs = all ((==4) . length) xs && length xs == (tourneyDiv tourneySize)
    pair :: Float -> Float -> [a] -> [a] -> [[a]]
    pair by acc xs ys | length xs + length ys == 4 = [xs++ys]
    pair by acc xs ys = (x++y) : pair by (by+acc-fromIntegral n) xs' ys'
        where
        n::Int = round (by+acc)
        (y,ys') = splitAt n ys
        (x,xs') = splitAt (4-n) xs

-- (tourney no, round no,match no,partial rankings)
type PlaySt p args db m a r = (HaapTourney p args db m a r,Int,Int,Int,HaapTourneySt' a)

type HaapPlay p args db m a r = StateT (PlaySt p args db m a r) (Haap p args (DB db) m)

playTourney :: (HaapMonad m,TourneyPlayer a) => HaapTourney p args db m a r -> [[a]] -> Int -> Int -> Haap p args (DB db) m (HaapTourneySt' a,TourneyTree a r)
playTourney tourney matches tourneyno tourneySize = do
    (tree,(_,_,_,_,rank)) <- State.runStateT
        (playRounds matches tourneySize tourneySize)
        (tourney,tourneyno,tourneySize,1,Map.empty)
    return (rank,tree)

playRounds :: (HaapMonad m,TourneyPlayer a) => [[a]] -> Int -> Int -> HaapPlay p args db m a r [Round a r]
playRounds matches tourneySize round = do
    (winners,roundRes) <- playRound matches tourneySize round
    case winners of
        [(winner,i)] -> return [roundRes]
        otherwise -> do
            roundRess <- playRounds (group4 $ map fst winners) tourneySize (nextRound round)
            return (roundRes:roundRess)

-- returns (standings for players that lost this round, winner players for next round)
playRound :: (HaapMonad m,TourneyPlayer a) => [[a]] -> Int -> Int -> HaapPlay p args db m a r ([(a,Int)],Round a r)
playRound matches tourneySize round = do
--    lift $ runIO $ putStrLn $ "playing round " ++ show round
    State.modify $ \(o,x,y,z,w) -> (o,x,round,1,w)
    (matches',roundRes) <- playMatches matches
    let uniquematches' = Map.toList $ foldr (\(a,x) m -> Map.insertWith (++) a [x] m) Map.empty (concat matches')    
--    lift $ runIO $ putStrLn $ "matches " ++ pretty (sort matches')
--    lift $ runIO $ putStrLn $ "umatches " ++ pretty (sort uniquematches')
    let winners = concatMap (take (roundWinners round)) matches'
    State.modify $ \(o,x,y,z,w) -> (o,x,y,z,addPlayerSts uniquematches' w)
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

playMatches :: HaapMonad m => TourneyPlayer a => [[a]] -> HaapPlay p args db m a r ([[(a,Int)]],[Match a r])
playMatches xs = do
    scores <- forM xs playMatch
    return (map fst scores,map (mapFst (map fst)) scores)

playMatch :: (HaapMonad m,TourneyPlayer a) => [a] -> HaapPlay p args db m a r ([(a,Int)],r)
playMatch xs = do
    (tourney,tourneyno,roundno,matchno,_) <- State.get
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


