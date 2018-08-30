{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHCJS.Fetch.Types
  ( JSRequest(..)
  , JSResponse(..)
  , JSHeaders(..)
  , JSPromise(..)
  , JSPromiseException(..)
  ) where

import Control.Exception
import Data.Typeable
import GHCJS.Marshal
import GHCJS.Types

newtype JSHeaders =
  JSHeaders JSVal

instance IsJSVal JSHeaders

newtype JSRequest = JSRequest JSVal

newtype JSPromise a = JSPromise JSVal

instance Show JSPromiseException where
  show _ = "PromiseException"

data JSPromiseException =
  JSPromiseException !JSVal
  deriving (Typeable)

instance Exception JSPromiseException

newtype JSResponse =
  JSResponse JSVal
  deriving (FromJSVal)
