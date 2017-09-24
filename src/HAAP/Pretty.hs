module HAAP.Pretty
    ( module HAAP.Pretty
    , module Text.PrettyPrint
    , module Text.PrettyPrint.GenericPretty
    ) where

import HAAP.Core

import Data.Map (Map(..))
import qualified Data.Map as Map

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

instance (Out a,Out b) => Out (Map a b) where
    docPrec i x = doc x
    doc xs = doc $ Map.toList xs

instance Out HaapException where
    docPrec i x = doc x
    doc (HaapException str) = text str
    doc (HaapTimeout _ i) = text "timeout after" <+> int i <+> text "seconds"
    doc (HaapIOException ex) = text (show ex)


