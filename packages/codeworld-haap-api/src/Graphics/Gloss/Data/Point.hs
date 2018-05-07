{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Data.Point where

import qualified CodeWorld as CW 

type Path = [Point]
type Point = (Float, Float)

pointInBox 
        :: Point 
        -> Point 
        -> Point -> Bool
        
pointInBox (x0, y0) (x1, y1) (x2, y2)
        =  x0 >= min x1 x2
        && x0 <= max x1 x2
        && y0 >= min y1 y2
        && y0 <= max y1 y2

pointToCW :: Point -> CW.Point
pointToCW (x,y) = (coordToCW x,coordToCW y)

coordToCW :: Float -> Double
coordToCW = realToFrac

pointFromCW :: CW.Point -> Point
pointFromCW (cwx,cwy) = (realToFrac cwx,realToFrac cwy)

pathToCW :: Path -> [CW.Point]
pathToCW = map pointToCW

pathFromCW :: [CW.Point] -> Path
pathFromCW = map pointFromCW

infixr .-.
(.-.) :: Point -> Point -> Point
(x1,y1) .-. (x2,y2) = (x1 - x2,y1 - y2)

infixr .+.
(.+.) :: Point -> Point -> Point
(x1,y1) .+. (x2,y2) = (x1 + x2,y1 + y2)

infixr .*.
(.*.) :: Point -> Point -> Point
(x1,y1) .*. (x2,y2) = (x1 * x2,y1 * y2)

infixr ./.
(./.) :: Point -> Point -> Point
(x1,y1) ./. (x2,y2) = (x1 / x2,y1 / y2)