module Mapa where

import Data.Char (isDigit)
import Data.List
import Tarefa1Lib

data State = St {
    size::Int, -- size
    br::[(Int,Int)], -- bricks
    stn::[(Int,Int)], -- extra stones (spiral)
    pws::[(Int,Int)], -- pwup + 
    fls::[(Int,Int)], -- pwup ! 
    bb :: [(Int,Int,Int,Int,Int)], -- bomb * x,y,player,rd,time
    players::[(Int,Int,Int,Int,Int)], -- player nr,x,y,pws,fls
    --- extra
    time::Float -- time left
}

instance Show State where
  show = unlines . showMap -- map2Str

dfTime n = fromIntegral $ (n-2)^3
ex = St 7 [(4,1),(5,1),(5,5)] [] [(4,1)] [(5,1)] [(4,3,0,1,10)] [(0,5,3,1,1)] (dfTime 7)
ex2 = getMap.lines $ "#########\n#       #\n# #?# # #\n#     ? #\n#?# # #?#\n# ?  ?  #\n# #?#?# #\n#  ??   #\n#########\n+ 3 3\n! 5 5\n* 7 7 1 1 10\n0 4 3 +\n1 7 7"
ex3 = ex2 {players = (players ex2)++[(2,1,1,0,0)]}
ex4 = (getMap $ mapa 11 0) {players = [(0,1,1,0,0)]}
ex5 = (getMap $ mapa 15 0) {players = [(0,1,13,0,0)]}
emptyMap = St 5 [] [] [] [] [] []

-- parsing and printing

map2Str :: State -> [String]
map2Str s = 
  addPl . addBB . addPU '!' fls . addPU '+' pws .
  addBr '?' (br s) .
  addBr '#' (stn s) .
  take (size s) .
  map (map celToStr) $
  drawMap [90,90..] (size s) 0
  where
    addBB l = l ++ map (\(x,y,p,r,t)->
                unwords ["*",show x,show y,show p,show r,show t])
                (sortBy ord$bb s)
    addPl l = l ++ map (\(x,y,v,w,z)->
                unwords [show x,show y,show v,replicate w '+'++replicate z '!'])
                (sort$players s)
    addPU c f l = l ++ map (\(x,y)->unwords [[c],show x,show y]) (f s)
    ord (x1,y1,_,_,_) (x2,y2,_,_,_) = compare (y1,x1) (y2,x2)


addBr ch list = addBr2 ch 0 (sort.map (\(x,y)->(y,x))$list)
  where addBr2 _ _ [] l = l 
        addBr2 _ _ _ [] = []
        addBr2 ch y1 ((y,x):brs) (line:ls)
          | y1==y = addBr2 ch y1 brs $ (map (\(c,n)->if n==x then ch else c) (zip line [0..])) : ls
          | y1<y  = line : (addBr2 ch (y1+1) ((y,x):brs) ls)

showMap s = (++["time: "++show (time s)]).addBbBoard.addPlBoard.map2Str $ s
  where
    addPlBoard = addBr '@' (getPlayerLocs s)
    addBbBoard = addBr '*' (getBombLocs s)


getMap :: [String] -> State
getMap s@(x:_) = St size (getBrs 0 s) [] (concatMap (getPw '+') s) (concatMap (getPw '!') s)
                         (concatMap getBB s) (concatMap getPl s) (dfTime size)
  where size = length x
        getBrs _ [] = []
        getBrs y (l:ls) = (map (\(_,x)->(x,y)) . filter ((=='?').fst) $  zip l [0..] ) ++
                          (getBrs (y+1) ls)
        getPw c (c2:l) | c == c2 = case map (read::String->Int) (words l) of
                                    [x,y] -> [(x,y)]
        getPw _ _  = []
        getBB ('*':l) = case map (read::String->Int) (words l) of
                          [x,y,p,r,t] -> [(x,y,p,r,t)] 
        getBB _ = []
        getPl (c:l) | isDigit c = case (words (c:l)) of
                          (p:x:y:pws) -> [(read p::Int,read x::Int,read y::Int,
                                           fst (getPw pws),snd (getPw pws))] 
            where getPw :: [String] -> (Int,Int)
                  getPw [] = (0,0)
                  getPw (x:_) = case (span (/='!') x) of 
                    (l,r) -> (length l,length r)

        getPl _ = []


instance (Num a, Num b) => Num (a,b) where
    fromInteger x = (fromInteger x, fromInteger x)
    (a,b) + (a',b') = (a + a', b + b')
    (a,b) - (a',b') = (a - a', b - b')
    (a,b) * (a',b') = (a * a', b * b')
    negate (a,b) = (negate a, negate b)
    abs (a,b) = (abs a, abs b)
    signum (a,b) = (signum a, signum b)

-- aux
setTime t st = st {time = fromIntegral t}
addTime t st = st {time = (time st)+t}
getPlayerLocs = map (\(_,x,y,_,_)->(x,y)) . players 
getPlayerLocsExcp pl st = [(x,y)| (p,x,y,_,_)<-players st, p/=pl ]
getBombLocs = map (\(x,y,_,_,_)->(x,y)) . bb 
bombAt xy = find (\(x,y,_,_,_)->(x,y)==xy) . bb
bombsBy pl = filter (\(_,_,pl',_,_)->pl==pl') . bb
stoneLocs st = [(x,y) | x<-[0..n],y<-[0..n], x==0 || y==0 || x==n || y==n || (even x)&&(even y)] ++ (stn st)
  where n = (size st)-1
getPlayer pl = find (\(p,x,y,a,b)->p==pl) . players  -- Maybe Player
hasBomb xy = elem xy . getBombLocs
hidePlayer xy st = st {players = filter (\(_,x,y,_,_)->xy/=(x,y)) (players st)}


