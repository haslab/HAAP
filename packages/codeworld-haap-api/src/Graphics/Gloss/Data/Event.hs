{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Data.Event where

import qualified CodeWorld as CW

import qualified Data.Text as Text

import Graphics.Gloss.Data.Point

data Event
    = EventKey Key KeyState (Float, Float)	 
    | EventMotion (Float, Float)	 
  deriving (Eq,Ord,Show)

eventToCW :: Event -> CW.Event
eventToCW (EventKey (Char c) toggle pos) = stringKeyToCW [c] toggle
eventToCW (EventKey (SpecialKey k) toggle pos) = specialKeyToCW k toggle
eventToCW (EventKey (MouseButton b) Down pos) = CW.MousePress (mouseButtonToCW b) (pointToCW pos)
eventToCW (EventKey (MouseButton b) Up pos) = CW.MouseRelease (mouseButtonToCW b) (pointToCW pos)
eventToCW (EventMotion p) = CW.MouseMovement (pointToCW p)

eventFromCW :: CW.Event -> Event
eventFromCW (CW.KeyPress k) = stringKeyFromCW (Text.unpack k) Down
eventFromCW (CW.KeyRelease k) = stringKeyFromCW (Text.unpack k) Up
eventFromCW (CW.MousePress b p) = EventKey (MouseButton $ mouseButtonFromCW b) Down (pointFromCW p)
eventFromCW (CW.MouseRelease b p) = EventKey (MouseButton $ mouseButtonFromCW b) Up (pointFromCW p)
eventFromCW (CW.MouseMovement p) = EventMotion (pointFromCW p)
eventFromCW e = error $ "eventFromCW: " ++ show e

stringKeyToCW :: String -> KeyState -> CW.Event
stringKeyToCW k Down = CW.KeyPress $ Text.pack k
stringKeyToCW k Up = CW.KeyRelease $ Text.pack k

stringKeyFromCW :: String -> KeyState -> Event
stringKeyFromCW [c] toggle = EventKey (Char c) toggle (0,0)
stringKeyFromCW str toggle = case specialKeyFromCWString str of
    Just k -> EventKey (SpecialKey k) toggle (0,0)
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
  deriving (Eq,Ord,Show)

specialKeyToCW :: SpecialKey -> KeyState -> CW.Event
specialKeyToCW k Down = CW.KeyPress (Text.pack $ specialKeyToCWString k)
specialKeyToCW k Up = CW.KeyRelease (Text.pack $ specialKeyToCWString k)

specialKeyToCWString :: SpecialKey -> String
specialKeyToCWString KeyUnknown	      = "Unknown"
specialKeyToCWString KeySpace         = "Space"
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

specialKeyFromCWString :: String -> Maybe SpecialKey
specialKeyFromCWString "Unknown"       = Just KeyUnknown	
specialKeyFromCWString "Space"         = Just KeySpace      
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
specialKeyFromCWString str = Nothing

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