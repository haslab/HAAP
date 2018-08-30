{-|
Module      : GHCJS.Fetch
Description : GHCJS bindings for the JavaScript Fetch API
Copyright   : (c) Moritz Kiefer, 2017
License     : BSD3
Maintainer  : moritz.kiefer@purelyfunctional.org
Stability   : experimental
Portability : GHCJS

This module provides bindings for JavaScript’s [Fetch
API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API).

The bindings deliberately stay close to the JavaScript API so most
documentation and tutorials should be easily translatable.

An introduction to the Fetch API can be found at
<https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch>.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module GHCJS.Fetch
  (
  -- * Fetch
    fetch, fetchByteString, fetchBinary, fetchBinaryFile
  -- * Request
  , Request(..)
  , RequestOptions(..)
  , defaultRequestOptions
  , RequestMode(..)
  , RequestCredentials(..)
  , RequestCacheMode(..)
  , RequestRedirectMode(..)
  , RequestReferrer(..)
  -- * Response
  , responseJSON
  , responseText
  , responseBlob
  , responseArrayBuffer
  , responseMutableArrayBuffer
  -- * Exceptions
  , JSPromiseException(..)
  ) where

import           Control.Exception
import           Data.Aeson (Value(..))
import qualified Data.ByteString.Char8 as Char8
import           Data.CaseInsensitive as CI
import           Data.Foldable
import qualified Data.JSString as JSString
import           Data.Typeable
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Types
import           JavaScript.Object (setProp, getProp)
import qualified JavaScript.Object as Object
import           JavaScript.Object.Internal (Object(..))
import           Network.HTTP.Types
import           System.IO.Unsafe

import           GHCJS.Fetch.FFI
import           GHCJS.Fetch.Types 
--import GHCJS.DOM.Types (ArrayBuffer(..))
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer (..),MutableArrayBuffer (..),unsafeFreeze)
import GHCJS.Buffer
import GHCJS.Foreign

import Data.Binary
import Data.String
import Data.ByteString
import qualified Data.ByteString.Lazy as BL

-- | See <https://developer.mozilla.org/en-US/docs/Web/API/Request/mode>.
data RequestMode
  = Cors
  | NoCors
  | SameOrigin
  | Navigate
  deriving (Show, Eq, Ord)

-- | See <https://developer.mozilla.org/en-US/docs/Web/API/Request/credentials>.
data RequestCredentials
  = CredOmit
  | CredSameOrigin
  | CredInclude
  deriving (Show, Eq, Ord)

-- | See <https://developer.mozilla.org/en-US/docs/Web/API/Request/cache>.
data RequestCacheMode
  = CacheDefault
  | NoStore
  | Reload
  | NoCache
  | ForceCache
  | OnlyIfCached
  deriving (Show, Eq, Ord)

-- | See <https://developer.mozilla.org/en-US/docs/Web/API/Request/redirect>.
data RequestRedirectMode
  = Follow
  | Error
  | Manual
  deriving (Show, Eq, Ord)

-- | See <https://developer.mozilla.org/en-US/docs/Web/API/Request/referrer>.
data RequestReferrer
  = NoReferrer
  | Client
  | ReferrerUrl !JSString
  deriving (Show, Eq, Ord)

-- | These options correspond to the @init@ argument to the [Request constructor](https://developer.mozilla.org/en-US/docs/Web/API/Request/Request).
data RequestOptions = RequestOptions
  { reqOptMethod :: !Method -- ^ See <https://developer.mozilla.org/en-US/docs/Web/API/Request/method>.
  , reqOptHeaders :: !RequestHeaders -- ^ See <https://developer.mozilla.org/en-US/docs/Web/API/Headers>.
  , reqOptBody :: !(Maybe JSVal) -- ^ Corresponds to the @body@ argument in <https://developer.mozilla.org/en-US/docs/Web/API/Headers>.
  , reqOptMode :: !RequestMode
  , reqOptCredentials :: !RequestCredentials
  , reqOptCacheMode :: !RequestCacheMode
  , reqOptRedirectMode :: !RequestRedirectMode
  , reqOptReferrer :: !RequestReferrer
  }

-- | See <https://developer.mozilla.org/en-US/docs/Web/API/Request>.
data Request = Request
  { reqUrl :: !JSString
  , reqOptions :: !RequestOptions
  }

-- | Default request options. These correspond to the default options
-- specified in the fetch standard.
defaultRequestOptions :: RequestOptions
defaultRequestOptions =
  RequestOptions
  { reqOptMethod = methodGet
  , reqOptHeaders = []
  , reqOptBody = Nothing
  , reqOptMode = Cors
  , reqOptCredentials = CredOmit
  , reqOptCacheMode = CacheDefault
  , reqOptRedirectMode = Follow
  , reqOptReferrer = Client
  }

requestHeadersJSVal :: RequestHeaders -> IO JSVal
requestHeadersJSVal headers = do
  headersObj <- js_newHeaders
  traverse_
    (\(name, val) -> do
       let name' = (JSString.pack . Char8.unpack . CI.original) name
           val' = (JSString.pack . Char8.unpack) val
       js_appendHeader headersObj name' val')
    headers
  pure (jsval headersObj)

instance PToJSVal RequestMode where
  pToJSVal mode =
    case mode of
      Cors -> jsval ("cors" :: JSString)
      NoCors -> jsval ("no-cors" :: JSString)
      SameOrigin -> jsval ("same-origin" :: JSString)
      Navigate -> jsval ("navigate" :: JSString)

instance ToJSVal RequestMode where
  toJSVal = pure . pToJSVal

instance PToJSVal RequestCredentials where
  pToJSVal cred =
    case cred of
      CredOmit -> jsval ("omit" :: JSString)
      CredSameOrigin -> jsval ("same-origin" :: JSString)
      CredInclude -> jsval ("include" :: JSString)

instance ToJSVal RequestCredentials where
  toJSVal = pure . pToJSVal

instance PToJSVal RequestCacheMode where
  pToJSVal cacheMode =
    case cacheMode of
      CacheDefault -> jsval ("default" :: JSString)
      NoStore -> jsval ("no-store" :: JSString)
      Reload -> jsval ("reload" :: JSString)
      NoCache -> jsval ("no-cache" :: JSString)
      ForceCache -> jsval ("force-cache" :: JSString)
      OnlyIfCached -> jsval ("only-if-cached" :: JSString)

instance ToJSVal RequestCacheMode where
  toJSVal = pure . pToJSVal

instance PToJSVal RequestRedirectMode where
  pToJSVal redirectMode =
    case redirectMode of
      Follow -> jsval ("follow" :: JSString)
      Error -> jsval ("error" :: JSString)
      Manual -> jsval ("manual" :: JSString)

instance ToJSVal RequestRedirectMode where
  toJSVal = pure . pToJSVal

instance PToJSVal RequestReferrer where
  pToJSVal referrer =
    case referrer of
      NoReferrer -> jsval ("no-referrer" :: JSString)
      Client -> jsval ("about:client" :: JSString)
      ReferrerUrl url -> jsval url

instance ToJSVal RequestReferrer where
  toJSVal = pure . pToJSVal

instance ToJSVal RequestOptions where
  toJSVal (RequestOptions { reqOptMethod
                          , reqOptBody
                          , reqOptHeaders
                          , reqOptMode
                          , reqOptCredentials
                          , reqOptCacheMode
                          , reqOptRedirectMode
                          , reqOptReferrer
                          }) = do
    obj <- Object.create
    setMethod obj
    setHeaders obj
    setBody obj
    setMode obj
    setCredentials obj
    setCacheMode obj
    setRedirectMode obj
    setReferrer obj
    pure (jsval obj)
    where
      setMethod obj =
        setProp
          "method"
          ((jsval . JSString.pack . Char8.unpack) reqOptMethod)
          obj
      setBody obj = traverse_ (\body -> setProp "body" body obj) reqOptBody
      setHeaders obj = do
        headers <- requestHeadersJSVal reqOptHeaders
        setProp "headers" headers obj
      setMode obj = setProp "mode" (pToJSVal reqOptMode) obj
      setCredentials obj =
        setProp "credentials" (pToJSVal reqOptCredentials) obj
      setCacheMode obj = setProp "cache-mode" (pToJSVal reqOptCacheMode) obj
      setRedirectMode obj = setProp "redirect" (pToJSVal reqOptRedirectMode) obj
      setReferrer obj = setProp "referrer" (pToJSVal reqOptReferrer) obj

toJSRequest :: Request -> IO JSRequest
toJSRequest (Request url opts) = do
  opts' <- toJSVal opts
  js_newRequest url opts'

-- | Throws 'JSPromiseException' when the request fails.
-- See <https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/fetch>.
fetch :: Request -> IO JSResponse
fetch req = do
  JSResponse <$> (await =<< js_fetch =<< toJSRequest req)

-- | Throws 'JSPromiseException' when decoding fails.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Body/json>.
responseJSON :: JSResponse -> IO Value
responseJSON resp = fromJSValUnchecked =<< await =<< js_responseJSON resp

-- | Throws 'JSPromiseException' when decoding fails.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Body/text>.
responseText :: JSResponse -> IO JSString
responseText resp =
  fromJSValUnchecked =<< await =<< js_responseText resp
  
responseBlob :: JSResponse -> IO JSVal
responseBlob resp =
  fromJSValUnchecked =<< await =<< js_responseBlob resp
 
responseMutableArrayBuffer :: JSResponse -> IO MutableArrayBuffer
responseMutableArrayBuffer resp =
  pFromJSVal <$> (await =<< js_responseMutableArrayBuffer resp)

responseArrayBuffer :: JSResponse -> IO ArrayBuffer
responseArrayBuffer resp =
  unsafeFreeze =<< responseMutableArrayBuffer resp

await :: JSPromise a -> IO JSVal
await p = do
  obj <- js_await p
  Just success <- fromJSVal =<< getProp "success" obj
  if success
    then getProp "val" obj
    else do
      err <- getProp "val" obj
      throwIO (JSPromiseException err)

fetchByteString :: Request -> IO ByteString
fetchByteString req = do
    response <- fetch req
    arr <- responseArrayBuffer response
    let bs = toByteString 0 Nothing $ createFromArrayBuffer arr
    return bs

fetchBinary :: Binary a => Request -> IO a
fetchBinary req = do
    bs <- fetchByteString req
    return $ decode (BL.fromStrict bs)
    
fetchBinaryFile :: Binary a => FilePath -> IO a
fetchBinaryFile file = do
    let url = fromString file
    let opts = defaultRequestOptions { reqOptHeaders = [("origin","http://example.com")] }
    let req = Request url opts
    fetchBinary req

