{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Interface.Pure.Animate where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display

-- TODO: missing screen and background
animate :: Display -> Color -> (Float -> Picture) -> IO ()
animate screen back go = CW.animationOf (return . displayCWPicture screen back . go . realToFrac)