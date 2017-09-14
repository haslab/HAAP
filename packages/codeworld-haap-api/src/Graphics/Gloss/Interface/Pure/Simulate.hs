{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Interface.Pure.Simulate where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display

-- TODO: missing screen and background
simulate :: Display -> Color -> model -> (model -> Picture) -> (Float -> model -> model) -> IO ()
simulate display back state draw go = CW.simulationOf state goCW drawCW
    where
    goCW t w = go (realToFrac t) w
    drawCW = displayCWPicture display back . draw