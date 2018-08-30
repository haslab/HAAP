{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, PackageImports, OverloadedStrings #-}

module Main where

import Data.JSString
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Data.Hashable

import GHCJS.Fetch
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
--import GHCJS.DOM.Types (ArrayBuffer(..))
import JavaScript.TypedArray.ArrayBuffer
import GHCJS.Buffer

import Debug.Hoed.Observe
import Debug.Hoed.CompTree
import Debug.Hoed.Render
import Debug.Hoed.CompTree.Exts
import "Hoed" Debug.Hoed



foreign import javascript unsafe "console.log($1);"
    consoleLog :: JSVal -> IO ()
    
foreign import javascript unsafe "console.log($1);"
    consoleLogArrayBuffer :: ArrayBuffer -> IO ()

main = do
    (ct::CompTree) <- fetchBinaryFile "../CompTree"
    
    val <- toJSVal $ show ct
    consoleLog(val);
    




