module Tarefa6_li1g100 where

import Data.List
import System.Random
import Avanca
import Mapa

bot :: [String] -> Int -> Int -> Maybe Char
bot mapa = botSt (getMap mapa)

botSt :: State -> Int -> Int -> Maybe Char
botSt st player ticks = do
   scores <- getScores player ticks st
   best <- headSafe.sort $ scores
   snd best

headSafe [] = Nothing
headSafe l = Just $ head l

getScores :: Int -> Int -> State -> Maybe [((Int,Int),Maybe Char)]
getScores player ticks st = do
  (pl,x,y,b,f) <- getPlayer player st
  let sc = [ (getCost ((x,y)+(getDir d)) pl st, d)
           | d<-Nothing:(map Just "UDLR") ]
  let inDanger = fst (getCost (x,y) pl st) > 0
  let bombCost = ((getCostB (x,y) pl b f st,0),Just 'B')
  let timeup = ticks <= ((size st)-2)^2

  return $ if not inDanger  then    -- if not in danger, consider dropping bomb or going to the center
             (if timeup then randomBomb ticks pl st ++ map (preferCenter st x y) sc
                             -- map (prefereOponent x y pl st) sc 
                        else bombCost:sc) 
           else --map reversePref sc -- prefer not going to bricks and players
                -- map (preferSpace (x,y) st) sc
                map (reversePref2 (x,y) st) sc -- prefer not going to bricks

randomBomb t pl st = if roll<1 then [((0,-5),Just 'B')] else []
  where roll = head.tail $ randomRs (0,(t+pl+(size st)) `div` 2) (mkStdGen (pl+t+(size st)))

getDir (Just 'L') = (-1,0)
getDir (Just 'R') = ( 1,0)
getDir (Just 'D') = (0, 1)
getDir (Just 'U') = (0,-1)
getDir _          = ( 0,0)


reversePref ((i,j),m) = ((i,-j),m)
-- reversePref st ((i,j),m) = if j==(2*((size st)-2)) then ((i,-j),m) else ((i,-j),m)

reversePref2 xy st ((i,_),m) = ((i,-j2),m)
  where j2 = targetBrIn (xy+(getDir m)) st

-- DO NOT use - inefficient - will timeout
preferSpace loc st ((i,_),Nothing) = ((i,0),Nothing)
preferSpace loc st ((i,_),d) = ((i,-space),d)
  where space = freeSpace [loc+(getDir d)] [loc] st

freeSpace [] _ _ = 0
freeSpace (xy:rest) been st = 1 + (freeSpace (rest++nexts) (xy:been) st)
  where
  nexts = [ xy+d
          | d<-[(-1,0),(0,-1),(0,1),(1,0)], canGo (xy+d) st, not (elem (xy+d) been) ]


preferCenter st x y ((i,j),d) =
  ((i, distanceTo ((x,y)+(getDir d)) test (2*((size st)-2)) st  ), d)
  -- ((i,targetLoc [((x,y)+(getDir d),0)] [] center st),d) 
  where n = size st
        hasCenter = odd (n`div`2)
        center = (n`div`2,n`div`2) + (if hasCenter then (0,0) else (-1,0))
        test p n | p == center = Just n
                 | otherwise   = Nothing


prefereOponent x y pl st ((i,j),d) = ((i,targetOp [((x,y)+(getDir d),0)] [] pl st),d)

targetOp [] _ _ st = 2 * ((size st)-2)
targetOp ((xy,n):rest) been pl st
  | xy `elem` (pws st) = n
  | xy `elem` (fls st) = n
  | xy `elem` (getPlayerLocsExcp pl st) = n
  | otherwise = targetOp (rest++nexts) (xy:been) pl st
  where
  nexts = [ (xy+d,n+1)
          | d<-[(-1,0),(0,-1),(0,1),(1,0)], canGo (xy+d) st, not (elem (xy+d) been) ]


-- | based on: can go, close to a soon-to-explode bomb, should place bomb...
getCost :: (Int,Int) -> Int -> State -> (Int,Int)
getCost to pl st
  | canGo to st = pref $ dieX to st
  | otherwise   = pref $ (size st)*2
  where pref n = (n, targetIn to pl st)


-- | penalty for being close to bombs
dieX xy st   = evalBomb xy (0,0) st +
               dieX2 xy (1,0) st +
               dieX2 xy (-1,0) st +
               dieX2 xy (0,1) st +
               dieX2 xy (0,-1) st

dieX2 xy ab@(a,b) s | canGo next s = (evalBomb next ab s) + (dieX2 next ab' s)
                    | otherwise  = 0
  where next = xy + (signum ab)
        ab' = if      (a>0) then (a+1,0)
              else if (a<0) then (a-1,0)
              else if (b>0) then (0,b+1)
              else               (0,b-1)
      
-- | penalty for having a bomb at (x,y) at a distance of (d1,d2)   
evalBomb (x,y) (d1,d2) s = case b of
    Just (_,_,_,r,t) -> 
      -- reached xy after going for d1d2, and
      -- found a bomb with radius r and timout t.
      --  - penalty is 0 IF radius<dist
      --  - urgent (+t*2) if time <= dist+2 
      let urg = if t <= (dist+2) then (size s)-t else 0 in
      if (r>=dist) then (r-dist+1)+urg else 0
    Nothing -> 0
  where b = bombAt (x,y) s
        dist = abs $ d1+d2

-- | cost to place a bomb: -1 if it can explode a brick/player
getCostB :: (Int,Int) -> Int -> Int -> Int -> State -> Int
getCostB xy pl b f st
  | (hasBrickPl xy f pl st) && (hasBombs pl b st) = -1
  | otherwise = 1

hasBombs pl b st = b >= (length (bombsBy pl st))

hasBrickPl :: (Int,Int) -> Int -> Int -> State -> Bool
hasBrickPl xy f pl st = or [findBrick xy (d*(f+1,f+1)) pl st | d<-[(-1,0),(0,-1),(0,1),(1,0)]]

findBrick :: (Int,Int) -> (Int,Int) -> Int -> State -> Bool
findBrick xy delta pl st
  | next `elem` (br st) = True
  | xy `elem` (getPlayerLocsExcp pl st) = True
  -- | any (\(p,x,y,_,_)->(p/=pl)&&((x,y)==xy)) (players st) = True -- finds player !pl in xy
  | delta == (0,0)    = False
  | otherwise         = findBrick next (delta+(signum delta)*(-1)) pl st
 where
  next = xy+(signum delta)

-- | distance to a target (bonus, brick, player)
targetIn :: (Int,Int) -> Int -> State -> Int
targetIn xy pl st
  = distanceTo xy test (2 * ((size st)-2)) st
  where test p n | hasBrickPl p 0 pl st = Just $ n+1 
                 | p `elem` (pws st) = Just n
                 | p `elem` (fls st) = Just n
                 | otherwise = Nothing

-- | distance to a target (bonus, brick, NOT player)
targetBrIn :: (Int,Int) -> State -> Int
targetBrIn xy st
  = distanceTo xy test (2 * ((size st)-2)) st
  where test p n | hasBrick p st = Just $ n+1 
                 | p `elem` (pws st) = Just n
                 | p `elem` (fls st) = Just n
                 | otherwise = Nothing
        hasBrick xy st = or [(xy+d)`elem`(br st) | d<-[(1,0),(-1,0),(0,1),(0,-1)]]
    

distanceTo :: (Int,Int) -> ((Int,Int) -> Int -> Maybe a) -> a -> State -> a
distanceTo xy test end st = distanceTo2 [(xy,0)] [] test end st
distanceTo2 [] _ _ end _ = end
distanceTo2 ((xy,n):rest) been test end st = 
  case (test xy n) of
    Just v  -> v
    Nothing -> distanceTo2 (rest++nexts) (xy:been) test end st
  where
  nexts = [ (xy+d,n+1)
          | d<-[(-1,0),(0,-1),(0,1),(1,0)], canGo (xy+d) st, not (elem (xy+d) been) ]

