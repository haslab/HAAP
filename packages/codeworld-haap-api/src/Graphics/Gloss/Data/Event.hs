{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Data.Event where

import qualified CodeWorld as CW

import qualified Data.Text as Text
import Data.Char

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Display

data Event
    = EventKey Key KeyState Modifiers (Float, Float)	 
    | EventMotion (Float, Float)	
    | EventResize (Int,Int) 
  deriving (Eq,Ord,Show)

-- TODO: support modifiers
data Modifiers
        = Modifiers
        { shift :: KeyState
        , ctrl  :: KeyState
        , alt   :: KeyState }
        deriving (Show, Eq, Ord)

noModifiers = Modifiers Up Up Up

eventToCW :: Display -> Event -> CW.Event
eventToCW display (EventKey (Char c) toggle _ pos) = stringKeyToCW [toUpper c] toggle
eventToCW display (EventKey (SpecialKey k) toggle _ pos) = specialKeyToCW k toggle
eventToCW display (EventKey (MouseButton b) Down _ pos) = CW.MousePress (mouseButtonToCW b) (pointToCWWithDisplay display pos)
eventToCW display (EventKey (MouseButton b) Up _ pos) = CW.MouseRelease (mouseButtonToCW b) (pointToCWWithDisplay display pos)
eventToCW display (EventMotion p) = CW.MouseMovement (pointToCWWithDisplay display p)
eventToCW display e = error $ "eventToCW: " ++ show e

eventFromCW :: Display -> CW.Event -> Event
eventFromCW display (CW.KeyPress k) = stringKeyFromCW (Text.unpack k) Down
eventFromCW display (CW.KeyRelease k) = stringKeyFromCW (Text.unpack k) Up
eventFromCW display (CW.MousePress b p) = EventKey (MouseButton $ mouseButtonFromCW b) Down noModifiers (pointFromCWWithDisplay display p)
eventFromCW display (CW.MouseRelease b p) = EventKey (MouseButton $ mouseButtonFromCW b) Up noModifiers (pointFromCWWithDisplay display p)
eventFromCW display (CW.MouseMovement p) = EventMotion (pointFromCWWithDisplay display p)
eventFromCW display e = error $ "eventFromCW: " ++ show e

stringKeyToCW :: String -> KeyState -> CW.Event
stringKeyToCW k Down = CW.KeyPress $ Text.pack k
stringKeyToCW k Up = CW.KeyRelease $ Text.pack k

stringKeyFromCW :: String -> KeyState -> Event
stringKeyFromCW " " toggle = EventKey (SpecialKey KeySpace) toggle noModifiers (0,0)
stringKeyFromCW [c] toggle = EventKey (Char $ toLower c) toggle noModifiers (0,0)
stringKeyFromCW str toggle = case specialKeyFromCWString str of
    Just k -> EventKey (SpecialKey k) toggle noModifiers (0,0)
    Nothing -> error $ "stringKeyFromCW " ++ show str

data Key
    = Char Char	 
    | SpecialKey SpecialKey	 
    | MouseButton MouseButton
  deriving (Eq,Ord,Show)
 
data SpecialKey
    = KeyUnknown	 
    | KeySpace	 
    | KeyEsc	 
    | KeyF1	 
    | KeyF2	 
    | KeyF3	 
    | KeyF4	 
    | KeyF5	 
    | KeyF6	 
    | KeyF7	 
    | KeyF8	 
    | KeyF9	 
    | KeyF10	 
    | KeyF11	 
    | KeyF12	 
    | KeyF13	 
    | KeyF14	 
    | KeyF15	 
    | KeyF16	 
    | KeyF17	 
    | KeyF18	 
    | KeyF19	 
    | KeyF20	 
    | KeyF21	 
    | KeyF22	 
    | KeyF23	 
    | KeyF24	 
    | KeyF25	 
    | KeyUp	 
    | KeyDown	 
    | KeyLeft	 
    | KeyRight	 
    | KeyTab	 
    | KeyEnter	 
    | KeyBackspace	 
    | KeyInsert	 
    | KeyNumLock	 
    | KeyBegin	 
    | KeyDelete	 
    | KeyPageUp	 
    | KeyPageDown	 
    | KeyHome	 
    | KeyEnd	 
    | KeyShiftL	 
    | KeyShiftR	 
    | KeyCtrlL	 
    | KeyCtrlR	 
    | KeyAltL	 
    | KeyAltR	 
    | KeyPad0	 
    | KeyPad1	 
    | KeyPad2	 
    | KeyPad3	 
    | KeyPad4	 
    | KeyPad5	 
    | KeyPad6	 
    | KeyPad7	 
    | KeyPad8	 
    | KeyPad9	 
    | KeyPadDivide	 
    | KeyPadMultiply	 
    | KeyPadSubtract	 
    | KeyPadAdd	 
    | KeyPadDecimal	 
    | KeyPadEqual	 
    | KeyPadEnter
    | KeyCodeWorld String
  deriving (Eq,Ord,Show)

specialKeyToCW :: SpecialKey -> KeyState -> CW.Event
specialKeyToCW k Down = CW.KeyPress (Text.pack $ specialKeyToCWString k)
specialKeyToCW k Up = CW.KeyRelease (Text.pack $ specialKeyToCWString k)

specialKeyToCWString :: SpecialKey -> String
specialKeyToCWString KeyUnknown	      = "Unknown"
specialKeyToCWString KeySpace         = " "
specialKeyToCWString KeyEsc	          = "Esc"
specialKeyToCWString KeyF1	          = "F1"
specialKeyToCWString KeyF2	          = "F2"
specialKeyToCWString KeyF3	          = "F3"
specialKeyToCWString KeyF4	          = "F4"
specialKeyToCWString KeyF5	          = "F5"
specialKeyToCWString KeyF6	          = "F6"
specialKeyToCWString KeyF7	          = "F7"
specialKeyToCWString KeyF8	          = "F8"
specialKeyToCWString KeyF9	          = "F9"
specialKeyToCWString KeyF10	          = "F10"
specialKeyToCWString KeyF11	          = "F11"
specialKeyToCWString KeyF12	          = "F12"
specialKeyToCWString KeyF13	          = "F13"
specialKeyToCWString KeyF14	          = "F14"
specialKeyToCWString KeyF15	          = "F15"
specialKeyToCWString KeyF16	          = "F16"
specialKeyToCWString KeyF17	          = "F17"
specialKeyToCWString KeyF18	          = "F18"
specialKeyToCWString KeyF19	          = "F19"
specialKeyToCWString KeyF20	          = "F20"
specialKeyToCWString KeyF21	          = "F21"
specialKeyToCWString KeyF22	          = "F22"
specialKeyToCWString KeyF23	          = "F23"
specialKeyToCWString KeyF24	          = "F24"
specialKeyToCWString KeyF25	          = "F25"
specialKeyToCWString KeyUp	          = "Up"
specialKeyToCWString KeyDown	      = "Down"
specialKeyToCWString KeyLeft	      = "Left"
specialKeyToCWString KeyRight	      = "Right"
specialKeyToCWString KeyTab	          = "Tab"
specialKeyToCWString KeyEnter	      = "Enter"
specialKeyToCWString KeyBackspace	  = "Backspace"
specialKeyToCWString KeyInsert	      = "Insert"
specialKeyToCWString KeyNumLock	      = "NumLock"
specialKeyToCWString KeyBegin	      = "Begin"
specialKeyToCWString KeyDelete	      = "Delete"
specialKeyToCWString KeyPageUp	      = "PageUp"
specialKeyToCWString KeyPageDown	  = "PageDown"
specialKeyToCWString KeyHome	      = "Home"
specialKeyToCWString KeyEnd	          = "End"
specialKeyToCWString KeyShiftL	      = "ShiftL"
specialKeyToCWString KeyShiftR	      = "ShiftR"
specialKeyToCWString KeyCtrlL	      = "CtrlL"
specialKeyToCWString KeyCtrlR	      = "CtrlR"
specialKeyToCWString KeyAltL	      = "AltL"
specialKeyToCWString KeyAltR	      = "AltR"
specialKeyToCWString KeyPad0	      = "Pad0"
specialKeyToCWString KeyPad1	      = "Pad1"
specialKeyToCWString KeyPad2	      = "Pad2"
specialKeyToCWString KeyPad3	      = "Pad3"
specialKeyToCWString KeyPad4	      = "Pad4"
specialKeyToCWString KeyPad5	      = "Pad5"
specialKeyToCWString KeyPad6	      = "Pad6"
specialKeyToCWString KeyPad7	      = "Pad7"
specialKeyToCWString KeyPad8	      = "Pad8"
specialKeyToCWString KeyPad9	      = "Pad9"
specialKeyToCWString KeyPadDivide	  = "PadDivide"
specialKeyToCWString KeyPadMultiply	  = "PadMultiply"
specialKeyToCWString KeyPadSubtract	  = "PadSubtract"
specialKeyToCWString KeyPadAdd	      = "PadAdd"
specialKeyToCWString KeyPadDecimal	  = "PadDecimal"
specialKeyToCWString KeyPadEqual	  = "PadEqual"
specialKeyToCWString KeyPadEnter      = "PadEnter"
specialKeyToCWString (KeyCodeWorld s) = s

specialKeyFromCWString :: String -> Maybe SpecialKey
specialKeyFromCWString "Unknown"       = Just KeyUnknown	
specialKeyFromCWString " "         = Just KeySpace      
specialKeyFromCWString "Esc"           = Just KeyEsc	        
specialKeyFromCWString "F1"            = Just KeyF1	        
specialKeyFromCWString "F2"            = Just KeyF2	        
specialKeyFromCWString "F3"            = Just KeyF3	        
specialKeyFromCWString "F4"            = Just KeyF4	        
specialKeyFromCWString "F5"            = Just KeyF5	        
specialKeyFromCWString "F6"            = Just KeyF6	        
specialKeyFromCWString "F7"            = Just KeyF7	        
specialKeyFromCWString "F8"            = Just KeyF8	        
specialKeyFromCWString "F9"            = Just KeyF9	        
specialKeyFromCWString "F10"           = Just KeyF10	        
specialKeyFromCWString "F11"           = Just KeyF11	        
specialKeyFromCWString "F12"           = Just KeyF12	        
specialKeyFromCWString "F13"           = Just KeyF13	        
specialKeyFromCWString "F14"           = Just KeyF14	        
specialKeyFromCWString "F15"           = Just KeyF15	        
specialKeyFromCWString "F16"           = Just KeyF16	        
specialKeyFromCWString "F17"           = Just KeyF17	        
specialKeyFromCWString "F18"           = Just KeyF18	        
specialKeyFromCWString "F19"           = Just KeyF19	        
specialKeyFromCWString "F20"           = Just KeyF20	        
specialKeyFromCWString "F21"           = Just KeyF21	        
specialKeyFromCWString "F22"           = Just KeyF22	        
specialKeyFromCWString "F23"           = Just KeyF23	        
specialKeyFromCWString "F24"           = Just KeyF24	        
specialKeyFromCWString "F25"           = Just KeyF25	        
specialKeyFromCWString "Up"            = Just KeyUp	        
specialKeyFromCWString "Down"          = Just KeyDown	    
specialKeyFromCWString "Left"          = Just KeyLeft	    
specialKeyFromCWString "Right"         = Just KeyRight	    
specialKeyFromCWString "Tab"           = Just KeyTab	        
specialKeyFromCWString "Enter"         = Just KeyEnter	        
specialKeyFromCWString "Backspace"     = Just KeyBackspace	
specialKeyFromCWString "Insert"        = Just KeyInsert	    
specialKeyFromCWString "NumLock"       = Just KeyNumLock	    
specialKeyFromCWString "Begin"         = Just KeyBegin	        
specialKeyFromCWString "Delete"        = Just KeyDelete	  
specialKeyFromCWString "PageUp"        = Just KeyPageUp	  
specialKeyFromCWString "PageDown"      = Just KeyPageDown  
specialKeyFromCWString "Home"          = Just KeyHome	      
specialKeyFromCWString "End"           = Just KeyEnd	          
specialKeyFromCWString "ShiftL"        = Just KeyShiftL	  
specialKeyFromCWString "ShiftR"        = Just KeyShiftR	  
specialKeyFromCWString "CtrlL"         = Just KeyCtrlL	      
specialKeyFromCWString "CtrlR"         = Just KeyCtrlR	      
specialKeyFromCWString "AltL"          = Just KeyAltL	      
specialKeyFromCWString "AltR"          = Just KeyAltR	      
specialKeyFromCWString "Pad0"          = Just KeyPad0	      
specialKeyFromCWString "Pad1"          = Just KeyPad1	      
specialKeyFromCWString "Pad2"          = Just KeyPad2	      
specialKeyFromCWString "Pad3"          = Just KeyPad3	      
specialKeyFromCWString "Pad4"          = Just KeyPad4	      
specialKeyFromCWString "Pad5"          = Just KeyPad5	      
specialKeyFromCWString "Pad6"          = Just KeyPad6	      
specialKeyFromCWString "Pad7"          = Just KeyPad7	      
specialKeyFromCWString "Pad8"          = Just KeyPad8	      
specialKeyFromCWString "Pad9"          = Just KeyPad9	      
specialKeyFromCWString "PadDivide"     = Just KeyPadDivide	    
specialKeyFromCWString "PadMultiply"   = Just KeyPadMultiply	
specialKeyFromCWString "PadSubtract"   = Just KeyPadSubtract	
specialKeyFromCWString "PadAdd"        = Just KeyPadAdd	      
specialKeyFromCWString "PadDecimal"    = Just KeyPadDecimal  
specialKeyFromCWString "PadEqual"      = Just KeyPadEqual	  
specialKeyFromCWString "PadEnter"      = Just KeyPadEnter      
specialKeyFromCWString str             = Just $ KeyCodeWorld str

data MouseButton
    = LeftButton	 
    | MiddleButton	 
    | RightButton	 
  deriving (Eq,Ord,Show)
 
mouseButtonToCW :: MouseButton -> CW.MouseButton
mouseButtonToCW LeftButton = CW.LeftButton
mouseButtonToCW MiddleButton = CW.MiddleButton
mouseButtonToCW RightButton = CW.RightButton
 
mouseButtonFromCW :: CW.MouseButton -> MouseButton
mouseButtonFromCW CW.LeftButton =   LeftButton
mouseButtonFromCW CW.MiddleButton = MiddleButton
mouseButtonFromCW CW.RightButton =  RightButton
 
data KeyState
    = Down	 
    | Up
  deriving (Eq,Ord,Show)
 
pointToCWWithDisplay :: Display -> Point -> CW.Point
pointToCWWithDisplay (Display screenx screeny) (glossx,glossy) = (realToFrac cwx,realToFrac cwy)
    where
    screenx2 = realToFrac screenx / 2
    screeny2 = realToFrac screeny / 2
    cwx = 10 * glossx / screenx2
    cwy = 10 * glossy / screeny2

pointFromCWWithDisplay :: Display -> CW.Point -> Point
pointFromCWWithDisplay (Display screenx screeny) (cwx,cwy) = (realToFrac glossx,realToFrac glossy)
    where
    screenx2 = realToFrac screenx / 2
    screeny2 = realToFrac screeny / 2
    glossx = cwx * screenx2 / 10
    glossy = cwy * screeny2 / 10


fitScreenPoint :: Display -> Display -> Point -> Maybe Point
fitScreenPoint (Display sx sy) (Display cx cy) (ix,iy) = if inDisplay then Just (cix,ciy) else Nothing
    where
    inDisplay = ix >= -cx2*scalexy && iy <= cx2*scalexy && iy >= -cy2*scalexy && iy <= cy2*scalexy
    cix = ix / scalexy
    ciy = iy / scalexy
    scalex = realToFrac sx / realToFrac cx
    scaley = realToFrac sy / realToFrac cy
    scalexy = min scalex scaley
    cx2 = realToFrac cx / 2
    cy2 = realToFrac cy / 2

fitScreenEvent :: Display -> Display -> Event -> Maybe Event
fitScreenEvent screen display (EventKey key toggle mods pos) = fmap (EventKey key toggle mods) $ fitScreenPoint screen display pos
fitScreenEvent screen display (EventMotion pos) = fmap (EventMotion) $ fitScreenPoint screen display pos



