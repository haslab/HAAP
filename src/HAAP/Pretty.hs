{-
HAAP: Haskell Automated Assessment Platform

This module defines a basic pretty printing interface for HAAP.
-}

{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module HAAP.Pretty
    ( module HAAP.Pretty
    , module Text.PrettyPrint
    , module Text.PrettyPrint.GenericPretty
    ) where

import HAAP.Core

import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Exception
import Control.Monad
import Control.DeepSeq

import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

instance (Out a,Out b) => Out (Map a b) where
    docPrec i x = doc x
    doc xs = doc $ Map.toList xs

instance Out SomeException where
    docPrec i x = doc x
    doc x = text (displayException x)

data Pretty a = Pretty { prettyPrint :: a -> Doc, prettyValue :: a }

instance NFData a => NFData (Pretty a) where
    rnf (Pretty _ a) = rnf a

instance Eq a => Eq (Pretty a) where
    x == y = prettyValue x == prettyValue y
instance Ord a => Ord (Pretty a) where
    compare x y = compare (prettyValue x) (prettyValue y)

instance Show (Pretty a) where
    show x = show $ (prettyPrint x) (prettyValue x)

instance Out (Pretty a) where
    docPrec i x = doc x
    doc (Pretty f x) = f x
    
class OutIO a where
    docIO :: a -> IO Doc
    
instance {-# OVERLAPPABLE #-} Out a => OutIO a where
    docIO = return . doc
    
data PrettyIO a = PrettyIO { prettyPrintIO :: a -> IO Doc, prettyValueIO :: a }

instance NFData a => NFData (PrettyIO a) where
    rnf (PrettyIO _ a) = rnf a

instance Eq a => Eq (PrettyIO a) where
    x == y = prettyValueIO x == prettyValueIO y
instance Ord a => Ord (PrettyIO a) where
    compare x y = compare (prettyValueIO x) (prettyValueIO y)

instance Show a => Show (PrettyIO a) where
    show x = show (prettyValueIO x)

instance OutIO (PrettyIO a) where
    docIO (PrettyIO f x) = f x
    
prettyIO :: OutIO a => a -> IO String
prettyIO a = liftM show $ docIO a



