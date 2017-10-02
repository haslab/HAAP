module HAAP.Pretty
    ( module HAAP.Pretty
    , module Text.PrettyPrint
    , module Text.PrettyPrint.GenericPretty
    ) where

import HAAP.Core

import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Exception

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

instance (Out a,Out b) => Out (Map a b) where
    docPrec i x = doc x
    doc xs = doc $ Map.toList xs

instance Out SomeException where
    docPrec i x = doc x
    doc x = text (displayException x)

