{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Data.Display where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import qualified CodeWorld as CW
import qualified CodeWorld.Picture as CW
import qualified CodeWorld.Driver as CW

getDisplay :: IO Display
getDisplay = do
    (x,y) <- CW.getSizeOf "screen"
    return $ Display (round x) (round y)

data Display = Display Int Int

-- | Convert pictures with coordinates x in [-screenx/2,screenx/2], y in [-screeny/2,screeny/2] to codeworld pictures in the x,y in [-10,10] plane.
fitCWPictureToScreen :: Display -> CW.Picture -> CW.Picture
fitCWPictureToScreen (Display cx cy) p = CW.scaled (10 / cx2) (10 / cy2) p
    where
    cx2 = realToFrac cx / 2
    cy2 = realToFrac cy / 2

displayCWPicture :: Display -> Color -> Picture -> CW.Picture
displayCWPicture screen@(Display cx cy) back p = fitCWPictureToScreen screen (pictureToCW $ Pictures [background,p])
    where
    cx2 = realToFrac cx / 2
    cy2 = realToFrac cy / 2
    background = Color back $ Polygon [(-cx2,-cy2),(-cx2,cy2),(cx2,cy2),(cx2,-cy2)]

-- fits a picture drawn for a display into a screen
fitScreenPicture :: Display -> Display -> Picture -> Picture
fitScreenPicture screen@(Display sx sy) display@(Display cx cy) pic = Scale scalexy scalexy pic
    where
    scalex = realToFrac sx / realToFrac cx
    scaley = realToFrac sy / realToFrac cy
    scalexy = min scalex scaley


