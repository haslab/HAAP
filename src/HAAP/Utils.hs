module HAAP.Utils where

import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Time.LocalTime

import System.FilePath
import System.FilePath.Find

import Text.Printf

instance Eq ZonedTime where
    x == y = zonedTimeToUTC x == zonedTimeToUTC y
instance Ord ZonedTime where
    compare x y = compare (zonedTimeToUTC x) (zonedTimeToUTC y)

printFloat :: Float -> Int -> String
printFloat f i = printf ("%."++show i++"f") f

printDouble :: Double -> Int -> String
printDouble f i = printf ("%."++show i++"f") f

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (x,y) = (f x,y)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (x,y) = (x,f y)

mapFst3 :: (a -> a') -> (a,b,c) -> (a',b,c)
mapFst3 f (x,y,z) = (f x,y,z)

mapSnd3 :: (b -> b') -> (a,b,c) -> (a,b',c)
mapSnd3 f (x,y,z) = (x,f y,z)

mapThr3 :: (c -> c') -> (a,b,c) -> (a,b,c')
mapThr3 f (x,y,z) = (x,y,f z)

mapFstM :: Monad m => (a -> m c) -> (a,b) -> m (c,b)
mapFstM f (x,y) = f x >>= \x' -> return (x',y)

mapSndM :: Monad m => (b -> m c) -> (a,b) -> m (a,c)
mapSndM f (x,y) = f y >>= \y' -> return (x,y')

fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x

snd3 :: (a,b,c) -> b
snd3 (x,y,z) = y

thr3 :: (a,b,c) -> c
thr3 (x,y,z) = z

fst4 :: (a,b,c,d) -> a
fst4 (x,y,z,w) = x

snd4 :: (a,b,c,d) -> b
snd4 (x,y,z,w) = y

thr4 :: (a,b,c,d) -> c
thr4 (x,y,z,w) = z

fou4 :: (a,b,c,d) -> d
fou4 (x,y,z,w) = w

averageList :: Fractional a => [a] -> a
averageList xs | length xs == 0 = 0
               | otherwise = sum xs / realToFrac (length xs)

zipLeft :: [a] -> [b] -> [(a,Maybe b)]
zipLeft [] [] = []
zipLeft (x:xs) [] = (x,Nothing) : zipLeft xs []
zipLeft [] (y:ys) = []
zipLeft (x:xs) (y:ys) = (x,Just y) : zipLeft xs ys

group4 :: [a] -> [[a]]
group4 [] = []
group4 (x1:x2:x3:x4:xs) = [x1,x2,x3,x4] : group4 xs

snoc :: [x] -> x -> [x]
snoc xs x = xs ++ [x]

foldr0 :: (a -> a -> a) -> [a] -> a -> a
foldr0 f [] x = x
foldr0 f xs x = foldr1 f xs

lookupMap :: Ord a => a -> Map a b -> b
lookupMap x xs = case Map.lookup x xs of
    Just y -> y
    Nothing -> error $ "lookupMap can't find key"

pathDepth :: [FilePath] -> Int
pathDepth [] = 0
pathDepth ("":xs) = pathDepth xs
pathDepth (".":xs) = pathDepth xs
pathDepth ("..":xs) = pred $ pathDepth xs
pathDepth ("/":xs) = pathDepth xs
pathDepth ("./":xs) = pathDepth xs
pathDepth ("../":xs) = pred $ pathDepth xs
pathDepth (x:xs) = succ $ pathDepth xs

dirToRoot :: FilePath -> FilePath
dirToRoot p = case joinPath $ replicate (pathDepth $ splitPath p) ".." of
    "" -> "."
    x -> x

fileToRoot :: FilePath -> FilePath
fileToRoot p = case joinPath $ replicate (pred $ pathDepth $ splitPath p) ".." of
    "" -> "."
    x -> x

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f [] = return False
anyM f (x:xs) = f x >>= \b -> if b then return b else anyM f xs

concatPaths [] = []
concatPaths [x] = x
concatPaths (x:xs) = x ++ ":" ++ concatPaths xs

unSplitOn :: [a] -> [[a]] -> [a]
unSplitOn tok [] = []
unSplitOn tok [x] = x
unSplitOn tok (x:xs) = x ++ tok ++ unSplitOn tok xs

compareSnd x y = compare (snd x) (snd y)
