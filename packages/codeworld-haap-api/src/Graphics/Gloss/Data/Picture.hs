{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Data.Picture where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Geometry.Angle

import qualified Data.Text as Text

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

pictureToCW :: Picture -> CW.Picture
pictureToCW Blank = CW.blank
pictureToCW (Polygon path) = CW.solidPolygon (pathToCW path)
pictureToCW (Line path) = CW.path (pathToCW path)
pictureToCW (Circle r) = CW.circle (coordToCW r)
pictureToCW (ThickCircle t r) = CW.thickCircle (coordToCW t) (coordToCW r)
pictureToCW (Arc b e r) = CW.arc (angleToCW b) (angleToCW e) (coordToCW r)
pictureToCW (ThickArc t b e r) = CW.thickArc (coordToCW t) (angleToCW b) (angleToCW e) (coordToCW r)
pictureToCW (Text str) = CW.text $ Text.pack str
pictureToCW (Color c p) = CW.colored (colorToCW c) (pictureToCW p)
pictureToCW (Translate x y p) = CW.translated (coordToCW x) (coordToCW y) (pictureToCW p)
pictureToCW (Rotate r p) = CW.rotated (angleToCW r) (pictureToCW p)
pictureToCW (Scale x y p) = CW.scaled (coordToCW x) (coordToCW y) (pictureToCW p)
pictureToCW (Pictures ps) = CW.pictures $ reverse $ map pictureToCW ps
