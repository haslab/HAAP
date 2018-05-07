{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Interface.Pure.Simulate where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display

-- TODO: missing screen and background
simulate :: Display -> Color -> Int -> model -> (model -> Picture) -> (Float -> model -> model) -> IO ()
simulate display back framerate state draw go = CW.simulationOf state (realToFrac framerate) goCW drawCW
    where
    goCW t w = return $ go (realToFrac t) w
    drawCW = return . displayCWPicture display back . draw