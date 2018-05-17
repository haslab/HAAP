module Avanca where

import Data.List
import Data.Maybe
import Tarefa1Lib
import Mapa

-- | advances a state (using the our internal notion of state)
avancaSts :: Float -> State -> State
avancaSts n s | n<=0 = s
              | n<1  = avancaEps n s
              | otherwise = avancaSts (n-1) (avancaSt s)
  where avancaEps n s | (floor $ (time s)-n) == (floor $ time s) = s {time = ((time s)-n)}
                      | otherwise = (avancaSt s) {time = ((time s)-n)}

avancaSt :: State -> State
avancaSt st = --error $ "area "++ show area
                    spiral (floor$time st) $ st {
                      bb = newBB2,         -- rm *
                      players = newPls,    -- rm players
                      br = (br  st)\\area, -- rm ?
                      pws= (pws st)\\(area\\(br st)), -- rm +
                      fls= (fls st)\\(area\\(br st)),  -- rm !
                      time= (time st)-1
                    }
  where expl = filter (\(x,y,p,r,t)->t==1) (bb st)
        area = getArea st (concatMap explode expl) (map (\(x,y,_,_,_)->(x,y)) expl)
        -- bombs
        newBB = (bb st) \\ expl
        newBB2 = map (reduceBomb area) newBB
        -- players
        newPls = filter (\(_,x,y,_,_)->not$elem (x,y) area) (players st) 
        -- aux
        explode (x,y,p,r,t) = [(a,b,x,y) | a<-[0,r,-r], b<-[0,r,-r],a/=b,a==0||b==0]
        reduceBomb area b@(x,y,p,r,t) | (x,y) `elem` area = (x,y,p,r,1)
                                      | otherwise = (x,y,p,r,t-1)

getArea :: State -> [(Int,Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
getArea _ [] l = l
getArea st ((0,0,x,y):t) l = getArea st t l
getArea st ((a,0,x,y):t) l | canGo (x1,y) st = getArea st ((a1,0,x1,y):t) ((x1,y):l)
                           | otherwise = getArea st t ((x1,y):l)
  where (a1,x1) = upd(a,x)
getArea st ((0,b,x,y):t) l | canGo (x,y1) st = getArea st ((0,b1,x,y1):t) ((x,y1):l)
                           | otherwise = getArea st t ((x,y1):l)
  where (b1,y1) = upd(b,y)
getArea st a@((_,_,x,y):t) l = error $ "strange state "++show a -- getArea st t ((x,y):l)

upd :: (Int,Int) -> (Int,Int)
upd (a,x) | a<0 = (a+1,x-1)
          | a>0 = (a-1,x+1)

-- | check if it is possible to go to a given coordinate (inside borders, no brick, no stone (including spiral))
canGo :: (Int,Int) -> State -> Bool
canGo (x,y) s = ((odd y) || (odd x)) &&
                x > 0 && y > 0 &&
                x < ((size s)-1) &&
                y < ((size s)-1) &&
                (not$elem (x,y) (br  s)) &&
                (not$elem (x,y) (stn s))

move :: (Int,Int) -> Int -> State -> State
move (x,y) pl s | (isJust p) && (canGo (x3,y3) s) = update (fromJust p) (x3,y3) s
  where p = find (\(i,_,_,_,_)->i==pl) (players s)
        Just (_,x2,y2,_,_) = p
        (x3,y3) = (x+x2,y+y2)
move _ _ s = s

update pl (x,y) s = s {
  players = delete pl (players s) ++ [newpl],
  pws = delete (x,y) (pws s),
  fls = delete (x,y) (fls s)}
  where (i,_,_,a,b) = pl
        newpl = (i,x,y,a+p,b+f)
        p = length (filter (==(x,y)) (pws s)) 
        f = length (filter (==(x,y)) (fls s)) 

-- dropBomb pl s | (isJust p) && ( (x3,y3) s) = update (fromJust p) (x3,y3) s
--   where p = find (\(i,_,_,_,_)->i==pl) (players s)
--         Just (_,x2,y2,_,_) = p
--         (x3,y3) = (x+x2,y+y2)
dropBomb _ s = s

spiral :: Int -> State -> State
spiral time st = traverseSpr blocked (1,1) st
  where n = size st
        blocked = max ((n-2)^2 - time + 1) 0 
        traverseSpr 0 _ st = st
        traverseSpr n pos st = traverseSpr (n-1) (nextPos pos (size st)) (block pos st) 

-- | block: drop a (spiral) block on top a given coordinate.
-- | It removes: players, bombs, power ups, and bricks
-- | It adds: stones.
block pos st =
  st  { players = filter (\(_,x,y,_,_)->(x,y)/=pos) (players st)
      , bb      = filter (\(x,y,_,_,_)->(x,y)/=pos) (bb st)
      , pws = delete pos (pws st)
      , fls = delete pos (fls st)
      , br  = delete pos (br  st)
      , stn = pos:(stn st)
      }

-- | find the next position in the spiral (given a position and a size)
nextPos (x,y) s | (x+1)>=y && ix> y = (x+1,y) -- upper part
                | x> y     && ix<=y = (x,y+1) -- right part
                | x<=y     && ix< y = (x-1,y) -- bottom part
                | (x+1)< y && ix>=y = (x,y-1) -- left part
                | otherwise = (x,y)
  where ix = s-x-1
