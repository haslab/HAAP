module Play where

import Text.XML.Light
import Haskassonne
import Next (pontua)
import Data.Maybe (fromJust, isNothing, isJust)
import System.Environment
import System.Exit
import Control.Monad (when)
import Data.List (sortBy, (\\), nubBy)
import Data.Ord (comparing)

bot :: Element -> IO Element
bot board = do
    let Just b = toBoard board
    t <- play greedybiased b
    return $ fromTile t

-- | Play greedy com bias para poupar meeples 
greedy = do entrada <- getContents
            let Just aux = parseXMLDoc entrada
                Just b = toBoard aux
            t <- play greedybiased b
            putStrLn $ showElement $ fromTile $ t

outra = do entrada <- getContents
           let Just aux = parseXMLDoc entrada
               Just b = toBoard aux
           t <- play potential b
           putStrLn $ showElement $ fromTile $ t

-- | Play aleatório
random = do entrada <- getContents
            let Just aux = parseXMLDoc entrada
                Just b = toBoard aux
            t <- playrandom b
            putStrLn $ showElement $ fromTile $ t          

-- | Play com estratégia aleatória
playrandom :: Board -> IO Tile
playrandom b = sorteia $ candidatos b

play :: (Board -> Tile -> Int) -> Board -> IO Tile
play p b = do let ts = sortBy (comparing (p b)) (candidatos b)
                  m = p b (head ts)
                  ts' = takeWhile (\t -> p b t == m) ts
              sorteia ts'

-- | Valida se uma jogada é válida
playok = do args <- getArgs
            input <- readFile (args!!0)
            let Just xml = parseXMLDoc input
            let Just board =  toBoard xml
            --putStrLn $ show board
            input <- if (args!!1 == "-") then getContents else readFile (args!!1)
            let xml = parseXMLDoc input
            when (isNothing xml) $ exitWith (ExitFailure 2)            
            --putStrLn $ show xml
            let play = toTile $ fromJust xml
            when (isNothing play) $ exitWith (ExitFailure 2)            
            when (not (possible board $ fromJust play)) $ exitWith (ExitFailure 2)
            return ()

greedybiased :: Board -> Tile -> Int
greedybiased b t =
  let i = proximo b - 1
      is = [0 .. njogadores b - 1] \\ [i]
      b' = b {terrain = terrain b ++ [t], next = Nothing}
      b'' = pontua b'
      m = maximum $ map (scores b'' !!) is
   in - (2*((scores b'' !! i) - m) - length (meeples b' (i+1)))

potential :: Board -> Tile -> Int
potential b t =
  let i = proximo b - 1
      is = [0 .. njogadores b - 1] \\ [i]
      b' = b {terrain = terrain b ++ [t], next = Nothing}
      b'' = pontua b'
      m = maximum $ map (scores b'' !!) is
   in - (scores b'' !! i)
