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
    | Image Float Float CW.Img
  deriving (Eq,Show)

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
pictureToCW (Image w h img) = CW.image (coordToCW w) (coordToCW h) img

makeImage :: Float -> Float -> String -> Picture
makeImage w h n = Image w h (CW.StringImg n)

loadImage :: Float -> Float -> FilePath -> IO Picture
loadImage w h p = do
    CW.Image cs w h n <- CW.loadImage (coordToCW w) (coordToCW h) p
    return $! Image (realToFrac w) (realToFrac h) n

loadImage' :: FilePath -> IO Picture
loadImage' p = do
    CW.Image cs w h n <- CW.loadImage' p
    return $! Image (realToFrac w) (realToFrac h) n
