{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}

module JavaScript.Web.Worker.Extras where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad

import GHCJS.Types 
import GHCJS.Foreign 
import GHCJS.Foreign.Callback
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import           Data.JSString.Text
import qualified Data.JSString
import           Data.Word
import           GHCJS.DOM
import           GHCJS.DOM.HTMLTextAreaElement
import           GHCJS.DOM.HTMLMediaElement as HTML
import           GHCJS.DOM.HTMLImageElement
import           GHCJS.DOM.NonElementParentNode
import           GHCJS.DOM.GlobalEventHandlers as Global
import           GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Node as Node
import           GHCJS.DOM.Document
import qualified GHCJS.DOM.DOMRect as DOMRect
import qualified GHCJS.DOM.DOMRectReadOnly as DOMRectRO
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM
import           GHCJS.DOM.MouseEvent
import           GHCJS.DOM.Types hiding (Event(..),Text(..))
import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Types

import qualified JavaScript.Array as Array
import           JavaScript.Object
import           JavaScript.Web.AnimationFrame
import qualified JavaScript.Web.Canvas as Canvas
import qualified JavaScript.Web.Canvas.Internal as Canvas
import qualified JavaScript.Web.Location as Loc
import qualified JavaScript.Web.MessageEvent as WS
import qualified JavaScript.Web.WebSocket as WS

import qualified JavaScript.Web.Worker as Worker
import qualified JavaScript.Web.MessageEvent as Worker


-- | A synchronous worker can be seen as a @MVar@ that stores a javascript delegated computation
data SyncWorker a b = SyncWorker { swFile :: FilePath, swInput :: MVar (Maybe a), swOutput :: MVar b, swWorker :: Worker.Worker }

-- | creates a new worker; server-side
newSyncWorker :: FromJSVal b => FilePath -> IO (SyncWorker a b)
newSyncWorker workerfile = do
    inp <- newMVar Nothing
    out <- newEmptyMVar
    worker <- Worker.create (toJSString workerfile)
    -- register a listener that puts any message from the worker into the mvar
    serverWorkerListen worker $ \msg -> case msg of
        Nothing -> takeMVar inp >> return ()
        Just result -> takeMVar inp >> putMVar out result
    return $ SyncWorker workerfile inp out worker

-- | gets the worker result; non-blocking; server-side
tryTakeSyncWorker :: SyncWorker a b -> IO (Maybe b)
tryTakeSyncWorker s = tryTakeMVar (swOutput s)

-- | gets the worker result; blocking; server-side
takeSyncWorker :: SyncWorker a b -> IO b
takeSyncWorker s = takeMVar (swOutput s)

-- | sets the worker input; blocking; server-side
tryPutSyncWorker :: ToJSVal a => SyncWorker a b -> a -> IO Bool
tryPutSyncWorker s a = do
    b <- tryPutMVar (swInput s) (Just a)
    when b $ do
        a' <- Control.Exception.evaluate $! a
        serverPostMessage (swWorker s) a'
    return b

-- | sets the worker input; blocking; server-side
putSyncWorker :: ToJSVal a => SyncWorker a b -> a -> IO ()
putSyncWorker s a = do
    putMVar (swInput s) (Just a)
    a' <- Control.Exception.evaluate $! a
    serverPostMessage (swWorker s) a'
    
-- | deploy a worker that synchronously listens to input and replies with outputs; client-side
runSyncWorker :: (FromJSVal a,ToJSVal b) => (a -> b) -> IO ()
runSyncWorker (f :: a -> b) = do
    clientWorkerListen $ \a -> do
        b <- Control.Exception.evaluate $! f a
        clientPostMessage (Just b::Maybe b)
    clientPostMessage (Nothing::Maybe b)

-- * FFI

-- ** server

foreign import javascript unsafe "$1.onmessage = $2;"
    js_server_worker_listen_message :: Worker.Worker -> Callback (JSVal -> IO ()) -> IO ()
    
foreign import javascript unsafe "$r = $1.data;"
    js_server_worker_get_data :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.postMessage($2);"
    js_server_worker_postMessage  :: Worker.Worker -> JSVal -> IO ()

serverPostMessage :: (ToJSVal a,MonadIO m) => Worker.Worker -> a -> m ()
serverPostMessage w v = liftIO $ do
    jsv <- toJSVal v
    js_server_worker_postMessage w jsv

serverWorkerListen :: (FromJSVal a,MonadIO m) => Worker.Worker -> (a -> IO ()) -> m ()
serverWorkerListen w f = liftIO $ do
    callback <- asyncCallback1 $ \e -> do
        v <- js_server_worker_get_data e
        Just a <- fromJSVal v
        f a
    js_server_worker_listen_message w callback

-- ** client

foreign import javascript unsafe "self.onmessage = function (e) { $1(e.data); };"
    js_client_setOnMessage :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "console.log($1);"
    js_client_worker_log :: JSString -> IO ()

foreign import javascript unsafe "onmessage = function (e) { console.log('recebi');};"
    js_client_worker_listen :: IO ()

foreign import javascript unsafe "onmessage = $1;"
    js_client_worker_listen_message :: Callback (JSVal -> IO ()) -> IO ()
    
foreign import javascript unsafe "$r = $1.data;"
    js_client_worker_get_data :: JSVal -> IO JSVal

foreign import javascript unsafe "postMessage($1);"
    js_client_worker_postMessage  :: JSVal -> IO ()

clientPostMessage :: (ToJSVal a,MonadIO m) => a -> m ()
clientPostMessage v = liftIO $ do
    jsv <- toJSVal v
    js_client_worker_postMessage jsv

clientWorkerListen :: (FromJSVal a,MonadIO m) => (a -> IO ()) -> m ()
clientWorkerListen f = liftIO $ do
    callback <- asyncCallback1 $ \dta -> do
        Just a <- fromJSVal dta
        f a
    js_client_setOnMessage callback

clientWorkerLog :: ToJSString s => s -> IO ()
clientWorkerLog s = js_client_worker_log (toJSString s)

