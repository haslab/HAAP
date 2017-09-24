{-# LANGUAGE ExplicitForAll #-}

-- | This game mode lets you manage your own input. Pressing ESC will not abort the program.
--   You also don't get automatic pan and zoom controls like with `display`.
module Graphics.Gloss.Interface.IO.Game
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , playIO
        , Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Event
import qualified CodeWorld as CW
import Control.Monad

-- | Play a game in a window, using IO actions to build the pictures. 
playIO  :: forall world
        .  Display                      -- ^ Display mode.
        -> Color                        -- ^ Background color.
        -> world                        -- ^ The initial world.
        -> (world -> IO Picture)        -- ^ An action to convert the world a picture.
        -> (Event -> world -> IO world) -- ^ A function to handle input events.
        -> (Float -> world -> IO world) -- ^ A function to step the world one iteration.
                                        --   It is passed the period of time (in seconds) needing to be advanced.
        -> IO ()

playIO display back start draw react step = CW.interactionOf start stepCW reactCW drawCW
    where
    stepCW f w = step (realToFrac f) w
    reactCW e w = react (eventFromCW e) w
    drawCW w = liftM (displayCWPicture display back) (draw w)
