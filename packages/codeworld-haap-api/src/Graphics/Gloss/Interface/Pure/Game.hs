{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Interface.Pure.Game where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Event

-- TODO: missing screen and background
play :: Display -> Color -> world -> (world -> Picture) -> (Event -> world -> world) -> (Float -> world -> world) -> IO ()
play display back start draw react step = CW.interactionOf start stepCW reactCW drawCW
    where
    stepCW f w = return $ step (realToFrac f) w
    reactCW e w = return $ react (eventFromCW e) w
    drawCW w = return $ displayCWPicture display back (draw w)



