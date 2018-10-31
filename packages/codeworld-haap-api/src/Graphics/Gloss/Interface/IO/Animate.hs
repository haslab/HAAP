
-- | Animate a picture in a window.
module Graphics.Gloss.Interface.IO.Animate
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , animateIO,animateFitScreenIO)
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import qualified CodeWorld as CW
import Control.Monad


-- | Open a new window and display the given animation.
--
--   Once the window is open you can use the same commands as with @display@.
--
animateIO 
        :: Display                -- ^ Display mode.
        -> Color                  -- ^ Background color.
        -> (Float -> IO Picture)  -- ^ Function to produce the next frame of animation. 
                                  --      It is passed the time in seconds since the program started.
        -> IO ()

animateIO screen back go = CW.animationOf (liftM (displayCWPicture screen back) . go . realToFrac)

animateFitScreenIO :: Display -> Display -> Color -> (Float -> IO Picture) -> IO ()
animateFitScreenIO screen display back go = CW.animationOf (liftM (displayCWPicture screen back . fitScreenPicture screen display) . go . realToFrac)
