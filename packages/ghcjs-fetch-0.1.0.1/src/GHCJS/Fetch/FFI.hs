{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Fetch.FFI
  ( js_newRequest
  , js_await
  , js_fetch
  , js_responseJSON
  , js_responseText
  , js_responseBlob
  , js_responseMutableArrayBuffer
  , js_newHeaders
  , js_appendHeader
  ) where

import GHC.Stack
import GHCJS.Fetch.Types
import GHCJS.Types
import JavaScript.Object
--import GHCJS.DOM.Types
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer (..),MutableArrayBuffer (..))

#ifdef ghcjs_HOST_OS
foreign import javascript safe "new Request($1, $2)" js_newRequest
               :: JSString -> JSVal -> IO JSRequest

foreign import javascript interruptible
               "$1.then(function(a) { $c({ 'val': a, 'success': true }); }, function(e) { $c({ 'val': e, 'success': false }); });"
               js_await :: JSPromise a -> IO Object

foreign import javascript safe "fetch($1)" js_fetch ::
               JSRequest -> IO (JSPromise JSResponse)

foreign import javascript safe "$1.json()" js_responseJSON ::
               JSResponse -> IO (JSPromise JSVal)

foreign import javascript safe "$1.text()" js_responseText ::
               JSResponse -> IO (JSPromise JSString)

foreign import javascript safe "$1.blob()" js_responseBlob ::
               JSResponse -> IO (JSPromise JSVal)

foreign import javascript safe "$1.arrayBuffer()" js_responseMutableArrayBuffer ::
               JSResponse -> IO (JSPromise MutableArrayBuffer)

foreign import javascript safe "new Headers()" js_newHeaders ::
               IO JSHeaders

foreign import javascript safe "$1.append($2, $3);" js_appendHeader
               :: JSHeaders -> JSString -> JSString -> IO ()

#else

ghcjsOnly :: HasCallStack => a
ghcjsOnly = error "This definition is only supported on ghcjs"

js_newRequest :: JSString -> JSVal -> IO JSRequest
js_newRequest = ghcjsOnly

js_await :: JSPromise a -> IO Object
js_await = ghcjsOnly

js_fetch :: JSRequest -> IO (JSPromise JSResponse)
js_fetch = ghcjsOnly

js_responseJSON :: JSResponse -> IO (JSPromise JSVal)
js_responseJSON = ghcjsOnly

js_responseText :: JSResponse -> IO (JSPromise JSString)
js_responseText = ghcjsOnly

js_responseBlob :: JSResponse -> IO (JSPromise JSVal)
js_responseBlob = ghcjsOnly

js_responseMutableArrayBuffer :: JSResponse -> IO (JSPromise MutableArrayBuffer)
js_responseMutableArrayBuffer = ghcjsOnly

js_newHeaders :: IO JSHeaders
js_newHeaders = ghcjsOnly

js_appendHeader :: JSHeaders -> JSString -> JSString -> IO ()
js_appendHeader = ghcjsOnly
#endif
