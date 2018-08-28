{-# LANGUAGE CPP, ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}

module Graphics.Gloss.Picture.Size
    (pictureSize)
    where

import Graphics.Gloss (Picture(..),Point(..),Display(..),Path(..))
import qualified Graphics.Gloss as Gloss

import Data.Monoid

pictureSize :: Picture -> (Int,Int)
pictureSize pic = (round xdim,round ydim)
    where
    ((x,y), xdim, ydim) = reduceBBs (map (boundingBox.bbox) (norm pic))

-- converts degrees to radians
rad :: Float -> Float
rad alpha = alpha * pi / 180

-- converts radians to degrees
deg :: Float -> Float
deg alpha = alpha * 180 / pi


type Elipsis = (Float,Float,Float,Float,Float,Float)
data BB = Elipsis Elipsis | Points [Point] deriving Show

bbox :: Picture -> BB
bbox Blank = Points []
bbox (Polygon l) = Points l
bbox (Line l) = Points l
bbox (Circle r) = Elipsis (circElipsis r)
bbox (ThickCircle _ _) = error "not handled"
bbox (Arc _ _ _) = error "not handled"
bbox (ThickArc _ _ _ _) = error "not handled"
bbox (Text _) = Points [] -- TODO: hack
#if defined(ghcjs_HOST_OS)
bbox (Image {}) = error "not flat!"
#else
bbox (Bitmap {}) = error "not flat!"
#endif
bbox (Color c p) = bbox p
bbox (Scale xs ys p) = case bbox p of
                        Elipsis e -> Elipsis (scaleElipsis xs ys e)
                        Points l -> Points (scalePath xs ys l)
bbox (Translate x y p) = case bbox p of
                          Elipsis e -> Elipsis (trlElipsis x y e)
                          Points l -> Points (trlPath x y l)
bbox (Rotate alpha p) = case bbox p of
                         Elipsis e -> Elipsis (rotElipsis alpha e)
                         Points l -> Points (rotPath alpha l)
bbox (Pictures _) = error "not flat!"

boundingBox :: BB -> (Point, Float, Float)
boundingBox (Elipsis e) = bbElipsis e
boundingBox (Points l) = bbPath l

reduceBB ((x1,y1),w1,h1) ((x2,y2),w2,h2) = ((xmin,ymin),xmax-xmin,ymax-ymin)
 where xmin = min x1 x2
       ymin = min y1 y2
       xmax = max (x1+w1) (x2+w2)
       ymax = max (y1+h1) (y2+h2)

reduceBBs :: [(Point,Float,Float)] -> (Point,Float,Float)
reduceBBs [] = ((0,0),0,0)
reduceBBs l = foldl1 reduceBB l

bbPath :: Path -> (Point, Float, Float)
bbPath [] = ((0,0),0,0)
bbPath l = ((xmin,ymin),xmax-xmin,ymax-ymin)
 where (xs,ys) = unzip l
       xmin = minimum xs
       ymin = minimum ys
       xmax = maximum xs
       ymax = maximum ys

bbElipsis :: Elipsis -> (Point,Float,Float)
bbElipsis (a,b,c,d,e,f) | dlt==0 = error "delta"
                        | otherwise = ((xl,yb),xr-xl,yt-yb)
 where dlt = 4*a*c - b^2
       xc = (b*e - 2*c*d) / dlt
       yc = (b*d - 2*a*e) / dlt
       xr = xc + sqrt ((2*b*e-4*c*d)^2+4*dlt*(e^2-4*c*f)) / (2*dlt)
       yt = yc + sqrt ((2*b*d-4*a*e)^2+4*dlt*(d^2-4*a*f)) / (2*dlt)
       xl = xc - sqrt ((2*b*e-4*c*d)^2+4*dlt*(e^2-4*c*f)) / (2*dlt)
       yb = yc - sqrt ((2*b*d-4*a*e)^2+4*dlt*(d^2-4*a*f)) / (2*dlt)

-- converts point to polar coordinates
convPolar :: Point -> (Float,Float)
convPolar (x,y) = (sqrt (x^2+y^2), atan2 y x)

convCart :: (Float,Float) -> Point
convCart (r,gama) = (r*cos gama,r*sin gama)

circElipsis :: Float -> Elipsis
circElipsis r = (1/r^2,0,1/r^2,0,0,-1)

rotPoint :: Float -> Point -> Point
rotPoint alpha (x,y) = convCart (r,gama + (rad (-alpha)))
 where (r,gama) = convPolar (x,y)
 
rotPath :: Float -> Path -> Path
rotPath alpha = map (rotPoint alpha)

rotElipsis :: Float -> Elipsis -> Elipsis
rotElipsis alpha (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where theta = rad (alpha)
       a' = a*(cos theta)^2 + b*sin theta*cos theta + c*(sin theta)^2
       b' = 2*(c-a)*sin theta*cos theta + b*((cos theta)^2-(sin theta)^2)
       c' = a*(sin theta)^2 - b*sin theta*cos theta + c*(cos theta)^2
       d' = d*cos theta + e*sin theta
       e' = -d*sin theta + e*cos theta
       f' = f

trlPoint :: Float -> Float -> Point -> Point
trlPoint x y (a,b) = (a+x,b+y)

trlPath :: Float -> Float -> Path -> Path
trlPath x y = map (trlPoint x y)

trlElipsis :: Float -> Float -> Elipsis -> Elipsis
trlElipsis x y (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where a' = a
       b' = b
       c' = c
       d' = d - 2*a*x - b*y
       e' = e - b*x - 2*c*y
       f' = f + a*x^2 + b*x*y + c*y^2 - d*x -e*y

scalePoint :: Float -> Float -> Point -> Point
scalePoint xs ys (x,y) = (x*xs,y*ys)

scalePath :: Float -> Float -> Path -> Path
scalePath xs ys = map (scalePoint xs ys)

scaleElipsis :: Float -> Float -> Elipsis -> Elipsis
scaleElipsis xs ys (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where a' = a / (xs^2)
       b' = b / (xs * ys)
       c' = c / (ys^2)
       d' = d / xs
       e' = e / ys
       f' = f
       
norm :: Picture -> [Picture]
norm (ThickCircle _ _) = error "not handled"
norm (Arc _ _ _) = error "not handled"
norm (ThickArc _ _ _ _) = error "not handled"
norm (Text _) = [] -- TODO: hack
norm (Color c p) = map (Color c) (norm p)
norm (Scale xs ys p) = map (Scale xs ys) (norm p)
norm (Translate x y p) = map (Translate x y) (norm p)
norm (Rotate alpha p) = map (Rotate alpha) (norm p)
norm (Pictures l) = concat (map norm l)
norm Blank = []
#if defined(ghcjs_HOST_OS)
norm (Image w h _) = [Gloss.rectangleWire (fromIntegral w) (fromIntegral h)]
#else
#if MIN_VERSION_gloss(1,13,0)
norm (Bitmap dta) = [Gloss.rectangleWire (fromIntegral w) (fromIntegral h)]
  where (w,h) = Gloss.bitmapSize dta
#else
norm (Bitmap w h _ _) = [Gloss.rectangleWire (fromIntegral w) (fromIntegral h)]
#endif
#endif
--norm (Circle 0) = [] --degenerated circle
norm pic = [pic] -- Polygon, Line, Circle
