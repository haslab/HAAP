{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Interface.Pure.Display where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display

-- TODO: missing screen and background
display :: Display -> Color -> Picture -> IO ()
display display back p = CW.drawingOf (displayCWPicture display back p)