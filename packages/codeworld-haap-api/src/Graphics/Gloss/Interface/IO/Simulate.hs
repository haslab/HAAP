{-# LANGUAGE RankNTypes #-}

-- We export this stuff separately so we don't clutter up the 
-- API of the Graphics.Gloss module.

-- | Simulate mode is for producing an animation of some model who's picture
--   changes over finite time steps. The behavior of the model can also depent
--   on the current `ViewPort`.
module Graphics.Gloss.Interface.IO.Simulate
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , simulateIO)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import qualified CodeWorld as CW
import Control.Monad


simulateIO :: forall model
        .  Display               -- ^ Display mode.
        -> Color                 -- ^ Background color.
        -> Int                   -- ^ Framerate
        -> model                 -- ^ The initial model.
        -> (model -> IO Picture) -- ^ A function to convert the model to a picture.
        -> (Float -> model -> IO model) 
                                 -- ^ A function to step the model one iteration. It is passed the 
                                 --     current viewport and the amount of time for this simulation
                                 --     step (in seconds).
        -> IO ()

simulateIO display back framerate state draw go = CW.simulationOf state (realToFrac framerate) goCW drawCW
    where
    goCW t w = go (realToFrac t) w
    drawCW = liftM (displayCWPicture display back) . draw
