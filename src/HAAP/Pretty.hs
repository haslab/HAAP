{-
HAAP: Haskell Automated Assessment Platform

This module defines a basic pretty printing interface for HAAP.
-}

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


data Pretty a = Pretty { prettyPrint :: (a -> Doc), prettyValue :: a }

instance Eq a => Eq (Pretty a) where
    x == y = prettyValue x == prettyValue y
instance Ord a => Ord (Pretty a) where
    compare x y = compare (prettyValue x) (prettyValue y)

instance Show (Pretty a) where
    show x = show $ (prettyPrint x) (prettyValue x)

instance Out (Pretty a) where
    docPrec i x = doc x
    doc (Pretty f x) = f x

