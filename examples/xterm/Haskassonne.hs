{-# LANGUAGE DeriveGeneric #-}
module Haskassonne where

import Text.XML.Light
import System.Random (randomRIO)
import Data.Maybe
import Data.List
import Data.Ord (comparing)
import Test.QuickCheck
import Data.Set (fromList)
import Control.DeepSeq
import GHC.Generics
import Safe

data Board = Board {terrain :: Terrain, scores :: Scores, next :: Maybe Shape}

-- | Instância de 'Eq' para 'Board': ignora o next

instance Eq Board where
    b1 == b2 = fromList (terrain b1) == fromList (terrain b2) && fromList (scores b1) == fromList (scores b2)


colorize :: String -> String
colorize [] = []
colorize ('.':t) = "\ESC[2;42m.\ESC[0m" ++ colorize t
colorize ('*':t) = "\ESC[2;43m*\ESC[0m" ++ colorize t
colorize ('O':t) = "\ESC[2;41mO\ESC[0m" ++ colorize t
colorize ('\n':t) = "\n\r" ++ colorize t
colorize (' ':t) = " " ++ colorize t
colorize (c:'*':t) = "\ESC[1;43m"++[c]++"\ESC[0m" ++ colorize ('*':t)
colorize (c:'O':t) = "\ESC[1;41m"++[c]++"\ESC[0m" ++ colorize ('O':t)
colorize (c:t) = "\ESC[1;42m"++[c]++"\ESC[0m" ++ colorize t

--  | Instância de 'Show' para 'Board'
instance Show Board where
    show b = "\ESC[2J"++colorize (terrain2ascii b) ++ "\n" ++ 
             unlines (map aux (zip (scores b) [1..])) ++ "\n" ++ n
        where n = if isJust (next b) 
                  then "A seguir joga o " ++ show (proximo b) ++ " a tile " ++ show (fromJust $ next b) ++ " (faltam " ++ show (36 - length (terrain b)) ++ " tiles)"
                  else ""
              aux (p,i) = "Jogador " ++ show i ++ ": " ++ show p ++ " (" ++ show (7 - length (meeples b i)) ++ " meeples disponiveis)"

type Terrain = [Tile]

data Point = Point {x :: Int, y :: Int}
             deriving (Generic,Show,Eq,Ord)
             
instance NFData Point

data Tile = Tile {shape :: Shape, point :: Point, orientation :: Orientation, follower :: Maybe Follower}
            deriving (Generic,Show,Eq,Ord)

instance NFData Tile


data Shape = B | C | E | N
            deriving (Generic,Show,Eq,Read,Ord)

instance NFData Shape

data Orientation = North | East | South | West
                   deriving (Generic,Eq,Enum,Ord)

instance NFData Orientation

instance Arbitrary Orientation where
    arbitrary = elements [North, East, South, West]

instance Show Orientation where
    show North = "N"
    show South = "S"
    show East  = "E"
    show West  = "W"

instance Read Orientation where
    readsPrec _ ('N':s) = [(North,s)]
    readsPrec _ ('S':s) = [(South,s)]
    readsPrec _ ('E':s) = [(East,s)]
    readsPrec _ ('W':s) = [(West,s)]

data Follower = Follower {player :: Int, profession :: Profession}
                deriving (Generic,Show,Eq,Ord)

instance NFData Follower

data Profession = Knight | Monk | Farmer
                  deriving (Generic,Eq,Ord)

instance NFData Profession

instance Show Profession where
    show Knight = "K"
    show Monk   = "M"
    show Farmer = "F"

instance Read Profession where
    readsPrec _ ('K':s) = [(Knight,s)]
    readsPrec _ ('F':s) = [(Farmer,s)]
    readsPrec _ ('M':s) = [(Monk,s)]

type Scores = [Score]
type Score = Int

-- Utilidades

-- | Testa se um tabuleiro está vazio
vazio :: Board -> Bool
vazio = null . terrain

findTile :: Board -> Point -> Maybe Tile
findTile b p = find (\t -> point t == p) (terrain b)

exists :: Board -> Point -> Bool
exists b = isJust . findTile b

ocupados :: Board -> [Point]
ocupados b = map point (terrain b)

vizinhos :: Point -> [Point]
vizinhos p = [Point (x p + 1) (y p), Point (x p - 1) (y p), Point (x p) (y p + 1), Point (x p) (y p - 1)]

rodeado :: Board -> Point -> Bool
rodeado b p = all (exists b) (vizinhos p)

njogadores :: Board -> Int
njogadores b = length $ scores b

proximo :: Board -> Int
proximo b = (length (terrain b) `mod` njogadores b) + 1

-- | Procura todos os followers
followers :: Board -> [(Point,Follower)]
followers b = [(point t, fromJust $ follower t) | t <- terrain b, isJust (follower t)]

-- | Procura os meeples de um jogador
meeples :: Board -> Int -> [Point]
meeples b n = [p | (p,f) <- followers b, player f == n]

-- | Procura peças conectadas por uma feature (tipo de follower)
connected :: Board -> Tile -> Profession -> [Tile]
connected b t p = aux [t] [] []
    where aux [] _ a = a
          aux (t:ts) v a | elem (point t) v = aux ts v a
                         | otherwise = let as = map (fromJust . findTile b) $ filter (exists b) (adjacentes t p)
                                           rs = aux as (point t:v) []
                                       in aux ts (v++map point rs) (t:a++rs)

-- | Lista potenciais vizinhos de um dado tipo
adjacentes :: Tile -> Profession -> [Point]
adjacentes _ Monk = []
adjacentes (Tile E p North _) Knight = [Point (x p) (y p + 1)]
adjacentes (Tile E p South _) Knight = [Point (x p) (y p - 1)]
adjacentes (Tile E p East _)  Knight = [Point (x p + 1) (y p)]
adjacentes (Tile E p West _)  Knight = [Point (x p - 1) (y p)]
adjacentes (Tile E p o f) Farmer = vizinhos p \\ adjacentes (Tile E p o f) Knight
adjacentes (Tile N p North _) Knight = [Point (x p) (y p + 1), Point (x p - 1) (y p)]
adjacentes (Tile N p South _) Knight = [Point (x p) (y p - 1), Point (x p + 1) (y p)]
adjacentes (Tile N p East _)  Knight = [Point (x p + 1) (y p), Point (x p) (y p + 1)]
adjacentes (Tile N p West _)  Knight = [Point (x p - 1) (y p), Point (x p) (y p - 1)]
adjacentes (Tile N p o f) Farmer = vizinhos p \\ adjacentes (Tile N p o f) Knight
adjacentes (Tile C p _ _) Knight = vizinhos p
adjacentes (Tile B p _ _) Farmer = vizinhos p
    
fechado :: Board -> Point -> Profession -> Bool
fechado _ _ Farmer = False
fechado b p Monk = all (exists b) $ quadrado p
fechado b p Knight = let Just t = findTile b p 
                     in all (exists b) $ concat $ map (flip adjacentes Knight) $ connected b t Knight

-- | Dado um ponto dá todos os pontos do quadrado do qual o ponto é o centro
quadrado :: Point -> [Point]
quadrado p = p : vizinhos p ++ [Point (x p + 1) (y p + 1), Point (x p - 1) (y p - 1), Point (x p - 1) (y p + 1), Point (x p + 1) (y p - 1)]


moda :: [Int] -> [Int]
moda l = let t = group (sort l)
             m = maximumNote "moda" (map length t)
         in [headNote "moda" x | x <- t, length x == m]

samecity :: Board -> Tile -> Tile -> Bool
samecity b t u = u `elem` connected b t Knight

-- | Determina as posições do tabuleiro onde se pode jogar
livres :: Board -> [Point]
livres b = filter (not . exists b) $ (Point 0 0 :) $ concat $ map vizinhos $ ocupados b

-- | Determina candidatos a jogadas: apenas usa as posições onde se pode jogar
candidatos :: Board -> [Tile]
candidatos b = let s = fromJust $ next b
                   n = proximo b
               in filter (possible b) [Tile s p o f | p <- livres b, o <- [North, East, South, West], f <- Nothing:[Just $ Follower n x | x <- [Monk,Knight,Farmer]]]

-- | Testa se é possível jogar uma determinada tile no tabuleiro actual
possible :: Board -> Tile -> Bool
possible b t  = let vs = (map (fromJust . findTile b) . filter (exists b) $ vizinhos (point t)) -- tiles vizinhas
                in isJust (next b) && -- existe next no board
                   shape t == fromJust (next b) && -- a shape da tile é a mesma pedida no next
                   not (exists b (point t)) && -- a posição não está já ocupada
                   (not (null vs) || point t == Point 0 0) && -- existem vizinhos ou a posição é a inicial
                   validFollower t && -- o follower colocado é possível na shape respectiva
                   all (match t) vs && -- a tile faz match com os vizinhos
                   if isJust (follower t)
                   then let f = fromJust (follower t)
                        in player f == proximo b && -- é o jogador correcto
                           length (meeples b (player f)) < 7 &&  -- o jogador ainda tem meeples disponíveis
                           length aux == 1 -- não existem meeples nas tiles acessíveis
                   else True
    where aux = let p = profession (fromJust (follower t))
                in filter (==p) $ map (profession . fromJust . follower) $ filter (isJust . follower) $ connected b t p

-- | Testa se uma tile faz match com uma das tiles vizinhas
match :: Tile -> Tile -> Bool
match t v = margem t (diff (point t) (point v)) == margem v (diff (point v) (point t))
    where diff t v | x t == x v = if y v > y t then North else South
                   | y t == y v = if x v > x t then East else West
          margem a o = (!!1) $ headNote "match" $ rotate (4 - fromEnum o) (tile2ascii a)

-- | Converte um 'Element' para um 'Board'
toBoard :: Element -> Maybe Board
toBoard e = do t <- findElement (qname "terrain") e
               s <- findElement (qname "scores") e
               return $ Board {
                terrain = catMaybes $ map toTile $ findElements (qname "tile") t, 
                scores = toScores $ findElements (qname "score") s, 
                next = fmap toNext $ findElement (qname "next") e
               }

fromBoard :: Board -> Element
fromBoard b = Element {
                elName = qname "board",
                elAttribs = [],
                elContent = map Elem $ [t, s] ++ n,
                elLine = Nothing
              }
    where t = Element {
                elName = qname "terrain",
                elAttribs = [],
                elContent = map (Elem . fromTile) (terrain b),
                elLine = Nothing
              }
          s = Element {
                elName = qname "scores",
                elAttribs = [],
                elContent = map Elem $ fromScores (scores b),
                elLine = Nothing
              }
          n = if isNothing (next b) 
              then [] 
              else [Element {
                      elName = qname "next",
                      elAttribs = [Attr {attrKey = qname "tile", attrVal = show (fromJust (next b))}],
                      elContent = [],
                      elLine = Nothing
                    }
                   ]

toNext :: Element -> Shape
toNext e = read $ fromJust $ findAttr (qname "tile") e

toScores :: [Element] -> [Score]
toScores l = map snd $ sortBy (comparing fst) $ map toScore l
    where toScore :: Element -> (Int,Int) 
          toScore e = (read $ fromJust $ findAttr (qname "player") e, read $ fromJust $ findAttr (qname "score") e)

fromScores :: [Score] -> [Element]
fromScores l = map fromScore $ zip [1..] l
    where fromScore :: (Int,Int) -> Element
          fromScore (j,p) = Element {
                              elName = qname "score",
                              elAttribs = [Attr {attrKey = qname "player", attrVal = show j},
                                           Attr {attrKey = qname "score", attrVal = show p}
                                          ],
                              elContent = [],
                              elLine = Nothing
                            }

-- | Converte um 'Element' para uma 'Tile'
toTile :: Element -> Maybe Tile
toTile e = do s <- findAttr (qname "type") e
              x <- findAttr (qname "x") e
              y <- findAttr (qname "y") e
              o <- findAttr (qname "orientation") e
              return $ Tile {
                shape = read s,
                point = Point (read x) (read y),
                orientation = read o,
                follower = findElement (qname "follower") e >>= toFollower           
              }

fromTile :: Tile -> Element
fromTile t = Element {
               elName = qname "tile",
               elAttribs = [Attr {attrKey = qname "type", attrVal = show (shape t)},
                            Attr {attrKey = qname "x", attrVal = show (x (point t))},
                            Attr {attrKey = qname "y", attrVal = show (y (point t))},
                            Attr {attrKey = qname "orientation", attrVal = show (orientation t)}
                           ],
               elContent = maybe [] (\f -> [Elem $ fromFollower f]) (follower t),
               elLine = Nothing
             }

-- | Converte um 'Element' para um 'Follower'
toFollower :: Element -> Maybe Follower
toFollower e = do p <- findAttr (qname "player") e
                  t <- findAttr (qname "type") e
                  return $ Follower {
                    player = read p,
                    profession = read t
                  }

fromFollower :: Follower -> Element
fromFollower f = Element {
                   elName = qname "follower",
                   elAttribs = [Attr {attrKey = qname "player", attrVal = show (player f)},
                                Attr {attrKey = qname "type", attrVal = show (profession f)}
                               ],
                   elContent = [],
                   elLine = Nothing
                 }

qname :: String -> QName
qname n = blank_name {qName = n}


-- | Testa se o follower se adequa à shape
validFollower :: Tile -> Bool
validFollower (Tile _ _ _ Nothing) = True
validFollower (Tile E _ _ p) = profession (fromJust p) == Knight || profession (fromJust p) == Farmer
validFollower (Tile N _ _ p) = profession (fromJust p) == Knight || profession (fromJust p) == Farmer
validFollower (Tile C _ _ p) = profession (fromJust p) == Knight
validFollower (Tile B _ _ p) = profession (fromJust p) == Monk || profession (fromJust p) == Farmer

terrain2ascii :: Board -> String
terrain2ascii b | vazio b = ""
                | otherwise = unlines $ concat $ map merge [[maybe emptytile tile2ascii (findTile b (Point x y)) | x <- [minx .. maxx]] | y <- [maxy, maxy-1 .. miny]]
    where minx = minimum $ map x $ ocupados b
          miny = minimum $ map y $ ocupados b
          maxx = maximum $ map x $ ocupados b
          maxy = maximum $ map y $ ocupados b
          emptytile = replicate 5 $ replicate 5 ' '
          merge :: [[String]] -> [String]
          merge l = map (\i -> concat $ map (!!i) l) [0..4]

-- | Dada uma matriz, a função 'rotate' roda a matriz um determinado número de vezes
--
-- prop> rotate 4 = id
rotate :: Int -> [[a]] -> [[a]]
rotate 0 = id
rotate n = aux . rotate (n-1) 
    where aux = map reverse . foldr (zipWith (:)) (repeat [])

prop_rotate_4 :: Property
prop_rotate_4 = forAll (elements [1..10]) $ \n -> forAll (square n :: Gen [[Int]]) $ \m -> rotate 4 m == m

square :: Arbitrary a => Int -> Gen [[a]]
square n = list n (list n arbitrary)

list :: Int -> Gen a -> Gen [a]
list 0 g = return []
list n g = do t <- list (n-1) g
              h <- g
              return (h:t)

tile2ascii :: Tile -> [String]
tile2ascii t = rotate (fromEnum $ orientation t) (shape2ascii (shape t) (follower t))

shape2ascii :: Shape -> Maybe Follower -> [String]
shape2ascii B Nothing = 
    [".....",
     "..O..",
     ".OOO.",
     "..O..",
     "....."]
shape2ascii B (Just (Follower i Monk)) = 
    [".....",
     "..O..",
     ".O" ++ show i ++ "O.",
     "..O..",
     "....."]
shape2ascii B (Just (Follower i Farmer)) = 
    [".."++ show i ++ "..",
     "..O..",
     ".OOO.",
     "..O..",
     "....."]
shape2ascii C Nothing = 
    ["*****",
     "*****",
     "*****",
     "*****",
     "*****"]
shape2ascii C (Just (Follower i Knight)) = 
    ["*****",
     "*****",
     "**" ++ show i ++ "**",
     "*****",
     "*****"]
shape2ascii E Nothing = 
    ["*****",
     ".***.",
     "..*..",
     ".....",
     "....."]
shape2ascii E (Just (Follower i Knight)) = 
    ["*****",
     ".*" ++ show i ++ "*.",
     "..*..",
     ".....",
     "....."]
shape2ascii E (Just (Follower i Farmer)) = 
    ["*****",
     ".***.",
     "..*..",
     ".....",
     ".." ++ show i ++ ".."]
shape2ascii N Nothing = 
    ["*****",
     "****.",
     "***..",
     "**...",
     "*...."]
shape2ascii N (Just (Follower i Knight)) = 
    ["*****",
     "*" ++ show i ++ "**.",
     "***..",
     "**...",
     "*...."]
shape2ascii N (Just (Follower i Farmer)) = 
    ["*****",
     "****.",
     "***..",
     "**." ++ show i ++ ".",
     "*...."]

-- | Sorteia um elemento de uma lista
sorteia :: [a] -> IO a
sorteia l = do n <- randomRIO (0, length l - 1)
               return (l!!n)
