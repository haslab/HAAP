{-# LANGUAGE ForeignFunctionInterface #-}

module JavaScript.Xterm where

import XtermArgs

import qualified Data.Text as T 
import qualified Data.Text.Lazy as LT (Text)

import Data.Typeable (Typeable)
import GHCJS.Types (JSVal(..), JSString)
import GHCJS.Foreign (jsNull, jsUndefined)
import GHCJS.Foreign.Callback (syncCallback, asyncCallback, syncCallback1, asyncCallback1, syncCallback2, asyncCallback2, OnBlocked(..))
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int64)
import Data.Word (Word, Word64)
import Data.Maybe (fromJust)
import Data.Traversable (mapM)
import Data.JSString.Text
import GHCJS.DOM.Types
import Control.Applicative ((<$>))
import GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import GHCJS.DOM.Enums
import           GHCJS.Foreign.Callback
import GHCJS.DOM.HTMLTextAreaElement
import GHCJS.DOM.KeyboardEvent

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Data.IORef
import Data.Char

newtype Terminal = Terminal { unTerminal :: JSVal }

instance Eq (Terminal) where
  (Terminal a) == (Terminal b) = js_eq a b

instance PToJSVal Terminal where
  pToJSVal = unTerminal

instance PFromJSVal Terminal where
  pFromJSVal = Terminal

instance ToJSVal Terminal where
  toJSVal = return . unTerminal

instance FromJSVal Terminal where
  fromJSVal = return . fmap Terminal . maybeJSNullOrUndefined

foreign import javascript unsafe "new window[\"Terminal\"]()"
        js_xterm_newTerminal :: IO Terminal
        
newTerminal :: (MonadIO m) => m Terminal
newTerminal = liftIO $ js_xterm_newTerminal

foreign import javascript unsafe "new window[\"Terminal\"]({cursorBlink: $1, rows: $2, cols: $3, scrollback: $4, tabStopWidth : $5 })"
        js_xterm_newTerminalWith :: Bool -> Int -> Int -> Int -> Int -> IO Terminal

newTerminalWith :: (MonadIO m) => TerminalOpts -> m Terminal
newTerminalWith opts = liftIO (js_xterm_newTerminalWith (cursorBlink opts) (rows opts) (cols opts) (scrollback opts) (tabStopWidth opts))

foreign import javascript unsafe "$1.open($2);"
    js_xterm_open :: Terminal -> Element -> IO ()
    
open :: MonadIO m => Terminal -> Element -> m () 
open t e = liftIO $ js_xterm_open t e
    
foreign import javascript unsafe "$1.write($2);"
    js_xterm_write :: Terminal -> JSString -> IO ()

write :: (ToJSString s,MonadIO m) => Terminal -> s -> m ()
write t txt = liftIO $ js_xterm_write t (toJSString txt)

foreign import javascript unsafe "$1.writeln($2);"
    js_xterm_writeln :: Terminal -> JSString -> IO ()
    
writeln :: (ToJSString s,MonadIO m) => Terminal -> s -> m ()
writeln t txt = liftIO $ js_xterm_writeln t (toJSString txt)

foreign import javascript unsafe "$r = $1.element"
    js_xterm_element :: Terminal -> IO Element

element :: MonadIO m => Terminal -> m Element
element t = liftIO $ js_xterm_element t

foreign import javascript unsafe "$r = $1.textarea"
    js_xterm_textarea :: Terminal -> IO HTMLTextAreaElement

textarea :: MonadIO m => Terminal -> m HTMLTextAreaElement
textarea t = liftIO $ js_xterm_textarea t


foreign import javascript unsafe "$1.on('data',$2);"
    js_xterm_onData :: Terminal -> Callback (JSVal -> IO ()) -> IO ()
    
onData :: MonadIO m => Terminal -> (T.Text -> IO ()) -> m ()
onData t call = liftIO $ do
    callback <- syncCallback1 ContinueAsync (call . textFromJSVal)
    js_xterm_onData t callback

foreign import javascript unsafe "$1.off('data',$2);"
    js_xterm_offData :: Terminal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.textarea.addEventListener('keydown',$2,true);"
    js_xterm_onKeydown :: Terminal -> Callback (JSVal -> IO ()) -> IO ()

onKeydown :: MonadIO m => Terminal -> (KeyboardEvent -> IO ()) -> m ()
onKeydown t call = liftIO $ do
    callback <- syncCallback1 ContinueAsync $ \v -> do
        Just e <- fromJSVal v
        call e
    js_xterm_onKeydown t callback

foreign import javascript unsafe "$1.textarea.removeEventListener('keydown',$2,true);"
    js_xterm_offKeydown :: Terminal -> Callback (JSVal -> IO ()) -> IO ()

textHasNewLine :: T.Text -> Bool
textHasNewLine = T.foldl (\b c -> b || c=='\n') False

getln :: MonadIO m => Terminal -> m T.Text
getln t = liftIO $ do
    end <- newEmptyMVar
    content <- newIORef T.empty
    let call1 = \e -> do
        c <- getKeyCode e
        let txt = T.singleton $ chr $ fromIntegral c
        atomicModifyIORef content $ \str -> (T.append str txt,())
        case c of
            13 -> putMVar end ()
            otherwise -> return ()
    callback1 <- syncCallback1 ContinueAsync $ \v -> do
        Just e <- fromJSVal v
        call1 e
    let go1 = js_xterm_onKeydown t callback1
    let go2 = do
        takeMVar end
        js_xterm_offKeydown t callback1
        releaseCallback callback1
    
    concurrently_ go1 go2
    writeln t (""::String);
    readIORef content
    

        







