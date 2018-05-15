module Next where

import Text.XML.Light
import Haskassonne
import Data.Maybe (isJust, fromJust, catMaybes, isNothing)
import Data.List (sort, group, (\\), nubBy)
import System.Environment
import System.Exit
import Control.Monad (when, unless)

-- | Programa que implementa o Next usando o 'avanca'
main = do entrada <- getContents
          let Just elem = parseXMLDoc entrada
              Just b = toBoard elem
          b' <- avanca b
          putStrLn $ showElement (fromBoard b')

-- | Valida se um tabuleiro é um próximo estado válido
nextok = do args <- getArgs
            input <- readFile (args!!0)
            let Just xml = parseXMLDoc input
            let Just board =  toBoard xml
            result <- avanca board
            --putStrLn $ show board
            input <- if (args!!1 == "-") then getContents else readFile (args!!1)
            let xml = parseXMLDoc input
            when (isNothing xml) $ exitWith (ExitFailure 2)            
            --putStrLn $ show xml
            let n = toBoard $ fromJust xml
            when (isNothing n) $ exitWith (ExitFailure 2)            
            when (fromJust n /= result) $ exitWith (ExitFailure 2)
            unless ((isNothing (next (fromJust n)) && null (possiveis board)) || (possivel board (fromJust (next (fromJust n))))) $ exitWith (ExitFailure 2)
            return ()

-- | Avança o estado do tabuleiro: se não houver peças que possam ser jogadas retorna um tabuleiro final sem next
avanca :: Board -> IO Board
avanca b | null (possiveis b) = return $ pontua $ b {next = Nothing}
         | otherwise = do n <- sorteia (possiveis b)
                          return $ pontua $ b {next = Just n}

-- | Testa se existem peças que possam ser jogas
possiveis :: Board -> [Shape]
possiveis b = let shapes = map shape $ terrain b
                  deck = replicate 6 B ++ replicate 2 C ++ replicate 18 E ++ replicate 10 N
                  faltam = deck \\ shapes
              in filter (possivel b) faltam

-- | Testa se uma determinada 'Shape' pode ser jogada num dado 'Board'
possivel :: Board -> Shape -> Bool
possivel b s = if null (terrain b) 
  then s == E 
  else not (null (candidatos $ b {next = Just s}))

-- | Actualiza as pontuações do 'Board', retirando os meeples que pontuam
pontua :: Board -> Board
pontua b | isJust (next b) = actualiza b [(p,f) | (p,f) <- followers b, fechado b p (profession f)]
         | otherwise = actualiza b (followers b)

-- | Actualiza as pontuações de uma sequência de meeples
actualiza :: Board -> [(Point,Follower)] -> Board
actualiza b [] = b
actualiza b ((p, Follower i Monk):t) =
    let points = sum $ map (\x -> if exists b x then 1 else 0) $ quadrado p
    in retira (incrementa (actualiza b t) i points) p
actualiza b ((p, Follower i Knight):t) = 
    let Just x = findTile b p
        ts = connected b x Knight
        fs = [t | t <- ts, isJust $ follower t, profession (fromJust $ follower t) == Knight]
        ps = map point fs
        is = moda $ map (player . fromJust . follower)  fs
        factor = if fechado b p Knight then 2 else 1
        points = length ts * factor
    in actualiza (foldl (\b i -> incrementa b i points) (foldl retira b ps) is) [x | x <- t, fst x `notElem` ps]
actualiza b ((p, Follower i Farmer):t) =
    let Just x = findTile b p
        ts = connected b x Farmer
        fs = [t | t <- ts, isJust $ follower t, profession (fromJust $ follower t) == Farmer]
        ps = map point fs
        is = moda $ map (player . fromJust . follower)  fs
        points = 3 * length (nubBy (samecity b) $ filter (\t -> shape t /= B && fechado b (point t) Knight) ts)
    in actualiza (foldl (\b i -> incrementa b i points) (foldl retira b ps) is) [x | x <- t, fst x `notElem` ps]

retira :: Board -> Point -> Board
retira b p = b {terrain = aux $ terrain b}
    where aux [] = []
          aux (h:t) | point h == p = Tile (shape h) (point h) (orientation h) Nothing : aux t
                    | otherwise = h : aux t

incrementa :: Board -> Int -> Int -> Board
incrementa b n p = b {scores = aux (scores b) (n-1) p}
    where aux l n x = take n l ++ [(l!!n) + x] ++ drop (n+1) l

