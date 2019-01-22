{-# LANGUAGE TypeSynonymInstances, Trustworthy, BangPatterns #-}

module Graphics.Gloss.Data.Picture where

import qualified CodeWorld as CW
import qualified CodeWorld.Picture as CW

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Angle

import qualified Data.Text as Text
import Control.Monad

--TODO: This does not work well for non-square windows with rotates!
data Picture
    = Blank
    | Polygon Path
    | Line Path	
    | Circle Float
    | ThickCircle Float Float
    | Arc Float Float Float	
    | ThickArc Float Float Float Float	
    | Text String	
    | Color Color Picture	
    | Translate Float Float Picture	
    | Rotate Float Picture	
    | Scale Float Float Picture	
    | Pictures [Picture]	
    | Image Int Int CW.Img
  deriving (Eq,Show)

instance Semigroup Picture where
  x <> y = Pictures [x,y]
instance Monoid Picture where
  mempty = Blank
  mappend x y = Pictures [x,y]
  mconcat xs = Pictures xs  


blank :: Picture
blank = Blank

polygon :: Path -> Picture
polygon = Polygon

line :: Path -> Picture
line = Line

circle :: Float -> Picture
circle = Circle

thickCircle :: Float -> Float -> Picture
thickCircle = ThickCircle

arc :: Float -> Float -> Float -> Picture
arc = Arc

thickArc :: Float -> Float -> Float -> Float -> Picture
thickArc = ThickArc

text :: String -> Picture
text = Text

color :: Color -> Picture -> Picture
color = Color

translate :: Float -> Float -> Picture -> Picture
translate = Translate

rotate :: Float -> Picture -> Picture
rotate = Rotate

scale :: Float -> Float -> Picture -> Picture
scale = Scale

pictures :: [Picture] -> Picture
pictures = Pictures

pictureToCW :: Picture -> CW.Picture
pictureToCW Blank = CW.blank
pictureToCW (Polygon path) = CW.solidPolygon (pathToCW path)
pictureToCW (Line path) = CW.path (pathToCW path)
pictureToCW (Circle r) = CW.circle (coordToCW r)
pictureToCW (ThickCircle t r) = CW.thickCircle (coordToCW t) (coordToCW r)
pictureToCW (Arc b e r) = CW.arc (angleToCW b) (angleToCW e) (coordToCW r)
pictureToCW (ThickArc t b e r) = CW.thickArc (coordToCW t) (angleToCW b) (angleToCW e) (coordToCW r)
pictureToCW (Text str) = CW.text $ Text.pack str
pictureToCW (Color c p) = CW.propagateColor (colorToCW c) (pictureToCW p)
pictureToCW (Translate x y p) = CW.translated (coordToCW x) (coordToCW y) (pictureToCW p)
pictureToCW (Rotate r p) = CW.rotated (angleToCW r) (pictureToCW p)
pictureToCW (Scale x y p) = CW.scaled (coordToCW x) (coordToCW y) (pictureToCW p)
pictureToCW (Pictures ps) = CW.pictures $ reverse $ map pictureToCW ps
pictureToCW (Image w h img) = CW.image w h img

loadImage :: FilePath -> IO Picture
loadImage f = do
    CW.Image _ w h img <- CW.loadImage f
    return $ Image w h img

loadImageById :: String -> IO Picture
loadImageById n = do
    CW.Image _ w h img <- CW.loadImageById n
    return $ Image w h img
    
loadSizedImageById :: Int -> Int -> String -> IO Picture
loadSizedImageById w h n = do
    CW.Image _ w h img <- CW.loadSizedImageById w h n
    return $ Image w h img

--makeImage :: String -> Picture
--makeImage n = Image (CW.StringImg n)


--loadImage :: Float -> Float -> FilePath -> IO Picture
--loadImage w h p = do
--    CW.Image cs w h n <- CW.loadImage (coordToCW w) (coordToCW h) p
--    return $! Image (realToFrac w) (realToFrac h) n
--
--loadImage' :: FilePath -> IO Picture
--loadImage' p = do
--    CW.Image cs w h n <- CW.loadImage' p
--    return $! Image (realToFrac w) (realToFrac h) n

-- Other Shapes ---------------------------------------------------------------
-- | A closed loop along a path.
lineLoop :: Path -> Picture
lineLoop []     = Line []
lineLoop (x:xs) = Line ((x:xs) ++ [x])


-- Circles and Arcs -----------------------------------------------------------
-- | A solid circle with the given radius.
circleSolid :: Float -> Picture
circleSolid r
        = thickCircle (r/2) r


-- | A solid arc, drawn counter-clockwise between two angles at the given radius.
arcSolid  :: Float -> Float -> Float -> Picture
arcSolid a1 a2 r
        = thickArc a1 a2 (r/2) r


-- | A wireframe sector of a circle.
--   An arc is draw counter-clockwise from the first to the second angle at
--   the given radius. Lines are drawn from the origin to the ends of the arc.
---
--   NOTE: We take the absolute value of the radius incase it's negative.
--   It would also make sense to draw the sector flipped around the
--   origin, but I think taking the absolute value will be less surprising
--   for the user.
--
sectorWire :: Float -> Float -> Float -> Picture
sectorWire a1 a2 r_
 = let r        = abs r_
   in  Pictures
        [ Arc a1 a2 r
        , Line [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , Line [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))] ]


-- Rectangles -----------------------------------------------------------------
-- NOTE: Only the first of these rectangle functions has haddocks on the
--       arguments to reduce the amount of noise in the extracted docs.

-- | A path representing a rectangle centered about the origin
rectanglePath
        :: Float        -- ^ width of rectangle
        -> Float        -- ^ height of rectangle
        -> Path
rectanglePath sizeX sizeY
 = let  sx      = sizeX / 2
        sy      = sizeY / 2
   in   [(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle centered about the origin.
rectangleWire :: Float -> Float -> Picture
rectangleWire sizeX sizeY
        = lineLoop $ rectanglePath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire :: Float -> Float -> Picture
rectangleUpperWire sizeX sizeY
        = lineLoop $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath :: Float -> Float -> Path
rectangleUpperPath sizeX sy
 = let  sx      = sizeX / 2
   in   [(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY
        = Polygon $ rectanglePath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY
        = Polygon  $ rectangleUpperPath sizeX sizeY
