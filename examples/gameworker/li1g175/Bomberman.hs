
module Bomberman where

import Control.Monad.State
import System.Random
import Data.Maybe
import Data.List

-- datatypes

data Mapa = M { grid :: [String]  
              , powers :: [Power]  
              , bombs :: [[Bomb]]  
              , players :: [Maybe Player]  
              } deriving (Eq)

data PowType = Flames | Bombs 
  deriving Eq

instance Show PowType where
    show Flames = "!"
    show Bombs = "+"

data Power = P { typ :: PowType, psp :: (Int,Int) }
  deriving Eq

instance Show Power where
    show (P s (x,y)) = show s ++ " " ++ show x ++ " " ++ show y
  
data Bomb = B { psb :: (Int,Int), ply :: Int, pow :: Int, tim :: Int }
  deriving Eq

instance Show Bomb where
    show (B (x,y) a b c) = "* " ++ show x ++ " " ++ show y ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c

data Player = Y { idp :: Int, psy :: (Int,Int), flm :: Int, bmb :: Int }
  deriving Eq

instance Show Player where
    show (Y p (x,y) fs bs) = show p ++ " " ++ show x ++ " " ++ show y ++ " " ++ (concat $ (map show (b++a)))
        where a = replicate (fs-1) Flames
              b = replicate (bs-1) Bombs

instance Show Mapa where
    show = unlines.printM

-- parsing and printing

{- prints a map divided by line -}
printM :: Mapa -> [String]
printM (M gs ps bs ys) = gs ++ (map show ps) ++ (concat $ map (map show) bs) ++ (map show $ catMaybes ys)

{- top-level parsing function -}
parse :: [String] -> Mapa
parse m = M m1 a (normalizeBombs b) c'
    where (m1,m2) = splitAt (length $ head m) m
          (a,b,c) = parseThings m2
          c' = normalizePlayers c

{- normalizes a bomb list per player -}
normalizeBombs :: [Bomb] -> [[Bomb]]
normalizeBombs b = [filter (flip ((==).ply) i) b | i <- [0..3]]

{- normalizes a player list, Nothing if non-existant -}
normalizePlayers :: [Player] -> [Maybe Player]
normalizePlayers b = map f [filter (flip ((==).idp) i) b | i <- [0..3]]
  where f [] = Nothing
        f (h:_) = Just h

{- parses the thing component of the map -}
parseThings :: [String] -> ([Power],[Bomb],[Player])
parseThings = foldr parseThing ([],[],[])
  where parseThing p@('!':_) (a,b,c) = (parsePower p:a,b,c)
        parseThing p@('+':_) (a,b,c) = (parsePower p:a,b,c)
        parseThing p@('*':_) (a,b,c) = (a,parseBomb p:b,c)
        parseThing p (a,b,c) = (a,b,parsePlayer p:c)

parsePower :: String -> Power
parsePower ('!':t) = P Flames (x,y)
    where x = read $ (words t)!!0
          y = read $ (words t)!!1
parsePower ('+':t) = P Bombs (x,y)
    where x = read $ (words t)!!0
          y = read $ (words t)!!1

parseBomb :: String -> Bomb
parseBomb (_:t) = B (x,y) a b c
    where x = read $ (words t)!!0
          y = read $ (words t)!!1
          a = read $ (words t)!!2
          b = read $ (words t)!!3
          c = read $ (words t)!!4

parsePlayer :: String -> Player
parsePlayer t = Y p (x,y) fs bs
    where p = read $ (words t)!!0
          x = read $ (words t)!!1
          y = read $ (words t)!!2
          ps = if (length (words t) > 3) then (words t)!!3 else []
          bs = (length $ filter (=='+') ps) + 1
          fs = (length $ filter (=='!') ps) + 1

-- player movement, T2

{- main movement function, moves a player or puts a bomb -}
movePlayerMain :: Mapa -> Int -> Char -> Mapa
movePlayerMain m@(M grid powers bombs players) p 'B' = (M grid powers (putBombs (players!!p) m p) players)
movePlayerMain m@(M grid powers bombs players) p d = catchPower p $ M grid powers bombs (movePlayers d m p)

{- catches powers if player moved on top of one -}
catchPower :: Int -> Mapa -> Mapa
catchPower i m = case (players m!!i) of
                   Nothing -> m
                   Just (Y _ pos _ _) ->
                     case getPower m pos of
                       Nothing -> m
                       Just pw' -> M (grid m) (delete pw' (powers m)) (bombs m) (applyPowers pw' m i)

{- moves a player with a given player id -}
movePlayers :: Char -> Mapa -> Int -> [Maybe Player]
movePlayers d = updatePlayerList (movePlayer d) players

{- moves a player particular player -}
movePlayer :: Char -> Mapa -> Maybe Player -> Maybe Player
movePlayer _ _ Nothing = Nothing
movePlayer d m (Just p@(Y idp pos flm bmb)) | canMove m p' = Just (Y idp p' flm bmb)
                                            | otherwise = Just p
  where p' = stepDir pos (dirFromDir d)

{- puts a bomb for a given player id -}
putBombs :: Maybe Player -> Mapa -> Int -> [[Bomb]]
putBombs p = updatePlayerList (putBomb p) bombs 

{- creates a bomb for a given player -} 
putBomb :: Maybe Player -> Mapa -> [Bomb] -> [Bomb]
putBomb Nothing _ bs = bs
putBomb (Just p@(Y idp (x,y) flm bmb)) m bs | canPut p (x,y-1) m = (B (x,y) idp flm 10):bs
                                            | otherwise = bs

{- applies the power to a given player id -}
applyPowers :: Power -> Mapa -> Int -> [Maybe Player]
applyPowers p = updatePlayerList (applyPower p) players 

{- applies a power to a player -} 
applyPower :: Power -> Mapa -> Maybe Player -> Maybe Player
applyPower p _ Nothing = Nothing
applyPower (P Flames _) _ (Just (Y i pos flm bmb)) = Just (Y i pos (flm+1) bmb)
applyPower (P Bombs _) _ (Just (Y i pos flm bmb)) = Just (Y i pos flm (bmb+1))

{- generic function to update player id indexed list -}
updatePlayerList :: (Mapa -> a -> a) -> (Mapa -> [a]) -> Mapa -> Int -> [a]
updatePlayerList f g m i = b1 ++ (f m (head b2)) : tail b2
  where (b1,b2) = splitAt i bs
        bs = g m

-- map evolution, T4

avancaMain :: Mapa -> Int -> Mapa
avancaMain m t = updateMap $ espiralaMapa t m

tickToStep :: Int -> Int -> Int
tickToStep sz tk = (sz - 2)^2 - tk + 1

{- atualiza as bombas e explode-as -}
updateMap :: Mapa -> Mapa
updateMap m = explodeMap m' (concat $ map (explosion m) bs)
    where (m',bs) = runState (updateBombs m) []

{- atualiza as bombas e saca as que têm que explodir -}
updateBombs :: Mapa -> State [Bomb] Mapa
updateBombs (M g ps bs ys) = do bs' <- mapM f bs
                                return (M g ps bs' ys)
    where f :: [Bomb] -> State [Bomb] [Bomb]
          f x = do x' <- mapM updateBomb x
                   return $ catMaybes x'                         

{- atualiza uma bomba e guarda-a no estado se tiver que explodir -}
updateBomb :: Bomb -> State [Bomb] (Maybe Bomb)
updateBomb bb@(B a b c d) | d <= 1 = do bs <- get
                                        put (bb:bs)
                                        return Nothing
                          | otherwise = return $ Just (B a b c (d-1))

{- calcula o impacto da explosão duma bomba, tendo em conta a grid -}
explosion :: Mapa -> Bomb -> [(Int,Int)]
explosion g (B (x,y) _ r _) = (x,y):(concat [expDir g (x,y) (r+1) i | i <- [0..3]])

explosionTime :: Mapa -> Int -> [(Int,Int)]
explosionTime m t = concat $ map (explosion m) bs
    where bs = filter ((flip ((==).tim) t)) (concat (bombs m))

{- calcula o impacto da explosão duma bomba num direção concreta, tendo em conta a grid -}
expDir :: Mapa -> (Int,Int) -> Int -> Int -> [(Int,Int)]
expDir _ _ 0 _ = []
expDir g (x,y) r d | (grid g)!!y!!x == '#' = []
                   | (grid g)!!y!!x == '?' = [(x,y)]
                   | getPower g (x,y) /= Nothing = [(x,y)]
                   | (grid g)!!y!!x == ' ' = (x,y):(expDir g (stepDir (x,y) d) (r-1) d)



{- atualiza o mapa dada o impacto das explosões -}
explodeMap :: Mapa -> [(Int,Int)] -> Mapa
explodeMap (M g p b y) es = (M g' p' b' y')
    where g' = explodeBricks g es
          p' = explodePowers g es p
          y' = explodePlayers es y
          b' = explodeBombs es b

{-- explode os tijolos dado o impacto das bombas (== esmagar) -}
explodeBricks :: [String] -> [(Int,Int)] -> [String]         
explodeBricks = explodeBricks' 0

explodeBricks' :: Int -> [String] -> [(Int,Int)] -> [String]
explodeBricks' _ [] _ = []
explodeBricks' y (h:t) es = explodeBricks'' (0,y) es h : explodeBricks' (y+1) t es

explodeBricks'' :: (Int,Int) -> [(Int,Int)] -> String -> String
explodeBricks'' _ _ [] = []
explodeBricks'' (x,y) es (h:t) | (x,y) `elem` es = ' ':rs
                               | otherwise = h:rs
  where rs = explodeBricks'' (x+1,y) es t

{-- explode os powerups dado o impacto das bombas -}
explodePowers :: [String] -> [(Int,Int)] -> [Power] -> [Power]         
explodePowers g es = filter cond
  where cond p@(P _ (x,y)) = (not $ (psp p) `elem` es) || g!!y!!x == '?'

{-- explode os players dado o impacto das bombas (== esmagar) -}
explodePlayers :: [(Int,Int)] -> [Maybe Player] -> [Maybe Player]       
explodePlayers es = map f
  where f Nothing = Nothing
        f (Just p) | not $ psy p `elem` es = Just p
                   | otherwise = Nothing

{-- explode as bombas dado o impacto das bombas -}
explodeBombs :: [(Int,Int)] -> [[Bomb]] -> [[Bomb]]
explodeBombs es = map (map bang)
    where bang bm@(B a b c d) | a `elem` es = B a b c 1
                              | otherwise = bm

{-- esmaga as bombas dado o impacto da espiral -}
smashBombs :: [[Bomb]] -> [(Int,Int)] -> [[Bomb]]       
smashBombs bs es = map (filter (\p -> not $ psb p `elem` es)) bs

{-- esmaga os powerups dado o impacto da espiral -}
smashPowers :: [(Int,Int)] -> [Power] -> [Power]         
smashPowers es = filter (\p -> not $ (psp p) `elem` es)

{- deixa cair uma pedra num mapa dado o # da pedra a cair -}
espiralaMapa :: Int -> Mapa -> Mapa
espiralaMapa t m@(M g ps bs ys) = case e of 
                                    Just e' -> let g' = (espiralaGrid 0 e' g)
                                                   ps' = smashPowers [e'] ps
                                                   bs' = smashBombs bs [e']
                                                   ys' = explodePlayers [e'] ys in
                                                M g' ps' bs' ys'
                                    _ -> m
    where e = espirala0 t (length g) 1


espiralaGrid :: Int -> (Int,Int) -> [String] -> [String]
espiralaGrid _ _ [] = []
espiralaGrid y e (h:t) | y == snd e = espiralaGrid' (0,y) e h : t
                       | otherwise = h : espiralaGrid (y+1) e t

espiralaGrid' :: (Int,Int) -> (Int,Int) -> String -> String
espiralaGrid' _ _ [] = []
espiralaGrid' (x,y) es (h:t) | (x,y) == es = '#':t
                             | otherwise = h:espiralaGrid' (x+1,y) es t


espirala0 :: Int -> Int -> Int -> Maybe (Int,Int)
espirala0 tk sz tm = espirala st 1 (sz - 2) (1,1)
  where st = (tickToStep sz tk) + tm - 1

espirala :: Int -> Int -> Int -> (Int,Int) -> Maybe (Int,Int)
espirala 1 _ _ p = Just p
espirala s d1 d2 (x,y) | y == d1 && x < d2 = espirala (s-1) d1 d2 (x+1,d1)
                       | y == d1 && x == d2 = espirala (s-1) d1 d2 (x,y+1)
                       | x == d2 && y < d2 = espirala (s-1) d1 d2 (x,y+1)
                       | x == d2 && y == d2 = espirala (s-1) d1 d2 (x-1,y)
                       | y == d2 && x > d1 = espirala (s-1) d1 d2 (x-1,y)
                       | y == d2 && x == d1 = espirala (s-1) d1 d2 (x,y-1)
                       | x == d1 && y > (d1+1) = espirala (s-1) d1 d2 (x,y-1)
                       | x == d1 && y == (d1+1) = espirala (s-1) (d1+1) (d2-1) (x+1,y)
                       | otherwise = Nothing







-- generate map, T1

mapa :: Int -> Int -> [String]
mapa d s = showMap m
    where (m,_) = runState (thingMap d 0) (randomThings d s)

showMap :: ([[Char]],[Power],[Power]) -> [String]
showMap (m,t1,t2) = m ++ x1 ++ x2
  where x1 = map show t1 
        x2 = map show t2 

thingMap :: Int -> Int -> State [Int] ([[Char]],[Power],[Power])
thingMap d 0 = do (a,b1,b2) <- thingMap d 1
                  return $ (replicate d '#' : a,b1,b2) 
thingMap d y | d-1 == y = return $ ([replicate d '#'],[],[])
             | otherwise = do
                rs <- get
                let ((k1,k2,k3),l) = runState (thingLine d (0,y)) rs
                put l
                (a,b1,b2) <- thingMap d (y+1)
                return $ (k1:a,k2++b1,k3++b2)

thingLine :: Int -> (Int,Int) -> State [Int] ([Char],[Power],[Power])
thingLine d (0,y) = do (a,b1,b2) <- thingLine d (1,y)
                       return $ ('#':a,b1,b2)
thingLine d (x,y) | d-1 == x = return $ (['#'],[],[])
                  | isCorner d (x,y) = do 
                      (a,b1,b2) <- thingLine d (x+1,y)
                      return (' ':a,b1,b2)
                  | isInnerStone (x,y) = do 
                      (a,b1,b2) <- thingLine d (x+1,y)
                      return ('#':a,b1,b2)
                  | otherwise = do 
                      rs <- get
                      let (c,bs1,bs2) = chooseThing (x,y) (head rs)
                      put $ tail rs
                      (a,b1,b2) <- thingLine d (x+1,y)
                      return (c:a,bs1++b1,bs2++b2)

isInnerStone :: (Int,Int) -> Bool
isInnerStone (x,y) = x `mod` 2 + y `mod` 2 == 0

isCorner :: Int -> (Int,Int) -> Bool
isCorner d (y,x) = (x `elem` [1,d-2] && y `elem` [1,2,d-2,d-3]) ||
                   (x `elem` [2,d-3] && y `elem` [1,d-2])

chooseThing :: (Int,Int) -> Int -> (Char, [Power], [Power])
chooseThing (x,y) r | r >= 0 && r <= 1 = ('?',[P Bombs (x,y)],[]) 
                    | r >= 2 && r <= 3 = ('?',[],[P Flames (x,y)]) 
                    | r >= 4 && r <= 39 = ('?',[],[]) 
                    | otherwise = (' ',[],[])

randomThings :: Int -> Int -> [Int]
randomThings n s | n < 7 = [] 
                 | otherwise = take x $ randomRs (0,99) (mkStdGen s)
    where x = ((n-2)^2) - 12 - (((n-3)^2) `div` 4)
    


-- basic functions

{- gets the closest valid position to the center that is the last to be smashed -}
center :: Mapa -> (Int,Int)
center (M g _ _ _) | odd c = (c,c)
                   | otherwise = (c-1,c)
    where c = (length g - 1) `div` 2

{- gets the power in a given position, if any -}
getPower :: Mapa -> (Int,Int) -> Maybe Power
getPower m x = listToMaybe f
    where g = grid m
          f = filter (flip ((==).psp) x) (powers m) 

{- gets the powers surrounding a given position -}
getSuroundingPowers :: Mapa -> (Int,Int) -> [Power]
getSuroundingPowers m p = catMaybes [getPower m (stepDir p i) | i <- [0..3]]    

{- gets the bomb in a given position, if any -}
getBomb :: Mapa -> (Int,Int) -> Maybe Bomb
getBomb bs x = listToMaybe f
    where f = filter (flip ((==).psb) x) bs'
          bs' = concat $ bombs bs

{- gets all players at a given position -}
getPlayer :: Mapa -> (Int,Int) -> [Player]
getPlayer m x = f
    where f = filter (flip ((==).psy) x) (catMaybes $ players m) 

{- gets all players surrounding a given position -}
getSuroundingPlayers :: Mapa -> (Int,Int) -> [Player]
getSuroundingPlayers m p = concat $ getPlayer m p : [getPlayer m (stepDir p i) | i <- [0..3]]

{- gets all players surrounding a given position except a given one -}
getSurPlayerExcept :: Int -> Mapa -> (Int,Int) -> [Player]
getSurPlayerExcept i m x = filter (flip ((/=).idp) i) ps
    where ps = getSuroundingPlayers m x

{- gets the box in a given position, if any -}
getBox :: Mapa -> (Int,Int) -> Maybe (Int,Int)
getBox m (x,y) | (grid m)!!y!!x == '?' = Just (x,y)
               | otherwise = Nothing
         
{- gets all boxes surrounding a given position -}
getSuroundingBoxes :: Mapa -> (Int,Int) -> [(Int,Int)]
getSuroundingBoxes m p = catMaybes [getBox m (stepDir p i) | i <- [0..3]]

{- whether a player can drop a bomb in a position -}
canPut :: Player -> (Int,Int) -> Mapa -> Bool
canPut (Y idp _ _ bmb) p m = length (bombs m!!idp) < bmb && isNothing (getBomb m p)
                
{- whether a player move to a position -}
canMove :: Mapa -> (Int,Int) -> Bool
canMove m (x,y) = not ((grid m)!!y!!x == '#' || (grid m)!!y!!x == '?')

{- maybe can move to a position -}
testMove :: Mapa -> (Int,Int) -> Maybe (Int,Int)
testMove g p | canMove g p = Just p
             | otherwise = Nothing

{- a list of valid moves available to a player -}
validMoves :: Mapa -> (Int,Int) -> [(Int,Int)]
validMoves g (x,y) = catMaybes [testMove g (stepDir (x,y) i) | i <- [0..3]] ++ [(x,y)]

{-| whether a position, after certain ticks, is safe (no explosions
  and no raining bricks)) |-}
safe :: Int -> Mapa -> (Int,Int) -> Int -> Bool
safe tk m pos t = let bs = filter (flip ((==).tim) t) (concat $ bombs m) 
                      ex = not (pos `elem` (concat $ map (explosion m) bs))
                      sp = (Just pos) /= (espirala0 tk sz t) in
                  ex && sp
  where sz = length $ grid m

{- increments a position in a givn direction -}
stepDir :: (Int,Int) -> Int -> (Int,Int)
stepDir (x,y) 0 = (x+1,y)
stepDir (x,y) 1 = (x,y+1)
stepDir (x,y) 2 = (x-1,y)
stepDir (x,y) 3 = (x,y-1)

{- maps dir ids into chars -}
dirToDir :: Int -> Char
dirToDir 0 = 'R'
dirToDir 1 = 'D'
dirToDir 2 = 'L'
dirToDir 3 = 'U'

dirFromDir :: Char -> Int
dirFromDir 'R' = 0 
dirFromDir 'D' = 1 
dirFromDir 'L' = 2 
dirFromDir 'U' = 3 

-- utils

distance :: (Int,Int) -> (Int,Int) -> Float
distance (a,b) (x,y) = sqrt $ toEnum $ (x-a)^2 + (y-b)^2 

deleteAll :: Eq a => [a] -> [a] -> [a]
deleteAll l [] = l
deleteAll l (h:t) = deleteAll (delete h l) t

-- examples

ex = M {
    grid =    ["###########",
               "#         #",
               "# # # # # #",
               "#         #",
               "# # # #?#?#",
               "#  ?   ? ?#",
               "# # # #?#?#",
               "#   ?    ?#",
               "# # # #?# #",
               "#   ?     #",
               "###########"],
    players = [Nothing, 
               Just (Y 1 (5,1) 1 1),
               Just (Y 2 (6,1) 1 1),
               Nothing],
    bombs =   [[B (7,1) 1 1 9],
               [B (5,3) 2 2 1],
               [],
               []],
    powers =  [P Flames (6,1)]
}


exs = M {
    grid =    ["######",
               "######",
               "# # ##",
               "#    #",
               "# # ##",
               "#   ##",
               "######"],
    players = [Nothing, 
               Just (Y 1 (3,4) 1 1),
               Just (Y 2 (3,4) 1 1),
               Nothing],
    bombs =   [[B (3,2) 1 1 9],
               [B (3,3) 2 2 3],
               [],
               []],
    powers =  []
}

ext = M {
    grid = ["###############",
            "###############",
            "# # # # # #?###",
            "#   ? ?? ? ? ##",
            "# #?# # #?# ###",
            "#          ??##",
            "#?# #?# # #?###",
            "# ?  ?       ##",
            "# # # # # #?###",
            "#? ??   ?  ? ##",
            "#?# #?# # #?# #",
            "#  ?    ? ? ? #",
            "# # # # #?#?# #",
            "#  ???    ?   #",
            "###############"],
    players = [Nothing, 
               Just (Y 1 (7,7) 1 1),
               Just (Y 2 (1,13) 1 1),
               Just (Y 3 (13,13) 1 1)],
    bombs =   [[],
               [],
               [],
               []],
    powers =  [P Flames (6,7)]  
}

ex1 = M {
    grid =    ["###########",
               "#         #",
               "# # # # # #",
               "#         #",
               "# # # #?#?#",
               "#      ? ?#",
               "# # # #?#?#",
               "#        ?#",
               "# # # #?# #",
               "#   ?     #",
               "###########"],
    players = [Nothing, 
               Just (Y 1 (5,5) 1 1),
               Just (Y 2 (5,3) 1 1),
               Nothing],
    bombs =   [[B (5,5) 1 1 3],
               [B (5,3) 1 2 7],
               [],
               []],
    powers =  [P Flames (6,1)]
}

exc = M {
    grid =    ["###########",
               "###########",
               "###########",
               "###########",
               "###########",
               "####  #####",
               "##### #####",
               "###########",
               "###########",
               "###########",
               "###########"],
    players = [Nothing, 
               Just (Y 1 (5,6) 1 1),
               Just (Y 2 (5,6) 1 1),
               Nothing],
    bombs =   [[],
               [B (5,6) 1 2 5],
               [B (5,5) 2 2 5],
               []],
    powers =  []
}

exa = M {
    grid =    ["###########",
               "###########",
               "###########",
               "###########",
               "### # #####",
               "###?    ###",
               "### # # ###",
               "###   ? ###",
               "###########",
               "###########",
               "###########"],
    players = [Just (Y 0 (7,5) 1 1),
               Just (Y 1 (7,5) 1 1),
               Just (Y 2 (7,5) 1 1),
               Nothing],
    bombs =   [[],
               [B (5,5) 1 1 6],
               []],
    powers =  []
}

ex9 = M {
    grid =    ["#########",
               "#########",
               "#########",
               "#########",
               "### #####",
               "###  ####",
               "#########",
               "#########",
               "#########"],
    players = [Just (Y 0 (3,5) 1 1),
               Just (Y 1 (3,5) 1 1),
               Just (Y 2 (3,5) 1 1),
               Nothing],
    bombs =   [[],
               [B (3,4) 1 1 6],
               []],
    powers =  []
}

exx = M {
    grid =    ["###################",
               "###################",
               "###################",
               "###################",
               "###################",
               "###################",
               "###################",
               "###################",
               "####### # #########",
               "########   ########",
               "######### #########",
               "###################",
               "###################",
               "###################",
               "###################",
               "###################",
               "###################",
               "###################",
               "###################"],
    players = [Just (Y 0 (9,8) 1 1),
               Just (Y 1 (9,8) 1 1),
               Just (Y 2 (9,10) 1 1),
               Nothing],
    bombs =   [[B (10,9) 1 1 6],
               [B (9,10) 1 1 6],
               []],
    powers =  []
}

exl = M {
    grid =    ["###############",
               "###############",
               "###############",
               "###############",
               "###############",
               "###############",
               "###############",
               "######  #######",
               "###############",
               "###############",
               "###############",
               "###############",
               "###############",
               "###############",
               "###############"],
    players = [Just (Y 0 (7,7) 1 1),
               Just (Y 1 (6,7) 1 1),
               Nothing,
               Nothing],
    bombs =   [[B (7,7) 1 2 6],
               [],
               [],
               []],
    powers =  []
}