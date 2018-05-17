module Tarefa1Lib where
  
import System.Environment
import Text.Read
import Data.Maybe
import System.Random

go s l = putStr $ unlines $ mapa s l

mapa :: Int -> Int -> [String]
mapa s seed =
    let rnd = randomRs (0,99) (mkStdGen seed)
        intMapa = drawMap rnd s 0
        pow = getPowerup intMapa 0
        bombs = [x | x@('+':_) <- pow]
        flames = [x | x@('!':_) <- pow]
        mapa = map (map celToStr) intMapa
    in mapa ++ bombs ++ flames

-- | draws a map
drawMap :: [Int] -> Int -> Int -> [[Int]]
drawMap r s 0 = (replicate s 101) : (drawMap r s 1)
drawMap r s n
    | n==(s-1) = [replicate s 101]
    | n==1 || n==(s-2) =
      (if s==5 then [101,100,100,100,101]
       else ([101,100,100]++(take (s-6) r)++[100,100,101]))
      : (drawMap (drop (s-6) r) s (n+1))
    | n==2 || n==(s-3) =
        let rndSize = (s-5)`div`2
            middle =  concatMap (\(x,y) -> [x,y]) $ take rndSize $ zip r [101,101..]
        in ([101,100,101]++middle++[100,101]) :
           (drawMap (drop rndSize r) s (n+1))
    | even n =
        let rndSize = (s-1)`div`2
            middle =  concatMap (\(x,y) -> [x,y]) $ take rndSize $ zip r [101,101..]
        in (101:middle) :
           (drawMap (drop rndSize r) s (n+1))
    | otherwise =
        let rndSize = (s-2)
            middle =  take rndSize r
        in (101:(middle++[101])) :
           (drawMap (drop rndSize r) s (n+1))
  
-- | get powerups
getPowerup :: [[Int]] -> Int -> [String]
getPowerup [] _ = []
getPowerup (line:rest) ind = (filter (/= "") (map (getPU ind) (zip line [0..]))) ++
                             (getPowerup rest (ind+1))
  where getPU y (n,x) | n==0 || n==1 = ("+ "++(show x)++" "++(show y))
                      | n==2 || n==3 = "! "++(show x)++" "++(show y)
                      | otherwise = ""

celToStr :: Int -> Char
celToStr 101 = '#'
celToStr n | n>=0 && n<40 = '?'
           | n>=40 = ' '
           | otherwise = error $ "found strange number '"++(show n)++"'"
