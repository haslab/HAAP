{-
HAAP: Haskell Automated Assessment Platform

This module defines a basic pretty printing interface for HAAP.
-}

{-# LANGUAGE RankNTypes, UndecidableInstances, FlexibleInstances #-}

module HAAP.Pretty
    ( module HAAP.Pretty
    , module Data.Text.Prettyprint.Doc
    ) where

import HAAP.Core

import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Data.Text (Text(..))

import Control.Exception (Exception,SomeException,displayException)
import Control.Monad
import Control.DeepSeq

import GHC.Stack

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Data.Text.Prettyprint.Doc.Render.Text

renderDocString :: Doc ann -> String
renderDocString = renderString . layoutPretty defaultLayoutOptions

renderDocText :: Doc ann -> T.Text
renderDocText = renderStrict . layoutPretty defaultLayoutOptions

prettyStringIO :: PrettyIO a => a -> IO String
prettyStringIO a = renderDocString <$> (prettyIO a)

prettyString :: Pretty a => a -> String
prettyString = renderDocString . pretty

prettyText :: Pretty a => a -> T.Text
prettyText = renderDocText . pretty

prettyTextIO :: PrettyIO a => a -> IO T.Text
prettyTextIO a = renderDocText <$> (prettyIO a)

instance Exception HaapException where
    displayException e = prettyString e

int :: Int -> Doc ann
int = pretty
text :: T.Text -> Doc ann
text = pretty
string :: String -> Doc ann
string = pretty
char :: Char -> Doc ann
char = pretty

($+$) :: Doc ann -> Doc ann -> Doc ann
x $+$ y = vcat[x,y]

instance Pretty HaapException where
    pretty (HaapException stack str) = haapCallStack stack $ text str
    pretty (HaapTimeout stack i) = haapCallStack stack $ string "timed out after" <+> int i <+> string "seconds"
    pretty (HaapIOException stack e) = haapCallStack stack $ string (displayException e)

instance Pretty HaapEvent where
    pretty (HaapEvent c s) = haapCallStack c (text s)
    
haapCallStack :: CallStack -> Doc ann -> Doc ann
haapCallStack c txt = vcat [string (prettyCallStack c) <> char ':',nest 4 txt]

instance (Pretty a,Pretty b) => Pretty (Map a b) where
    pretty xs = pretty $ Map.toList xs

instance Pretty SomeException where
    pretty x = string (displayException x)

data Prettyprint a = Prettyprint { prettyprint :: forall ann . a -> Doc ann, prettyValue :: a }

instance NFData a => NFData (Prettyprint a) where
    rnf (Prettyprint _ a) = rnf a

instance Eq a => Eq (Prettyprint a) where
    x == y = prettyValue x == prettyValue y
instance Ord a => Ord (Prettyprint a) where
    compare x y = compare (prettyValue x) (prettyValue y)

instance Show a => Show (Prettyprint a) where
    show (Prettyprint f v) = show v

instance Pretty (Prettyprint a) where
    pretty (Prettyprint f x) = f x
    
class PrettyIO a where
    prettyIO :: a -> IO (Doc ann)
    
instance {-# OVERLAPPABLE #-} Pretty a => PrettyIO a where
    prettyIO = return . pretty
    
data PrettyprintIO a = PrettyprintIO { prettyprintIO :: forall ann . a -> IO (Doc ann), prettyValueIO :: a }

instance NFData a => NFData (PrettyprintIO a) where
    rnf (PrettyprintIO _ a) = rnf a

instance Eq a => Eq (PrettyprintIO a) where
    x == y = prettyValueIO x == prettyValueIO y
instance Ord a => Ord (PrettyprintIO a) where
    compare x y = compare (prettyValueIO x) (prettyValueIO y)

instance Show a => Show (PrettyprintIO a) where
    show (PrettyprintIO f x) = show x

instance PrettyIO (PrettyprintIO a) where
    prettyIO (PrettyprintIO f x) = f x
   
instance Pretty ZonedTime where
    pretty = viaShow 

instance Pretty NominalDiffTime where
    pretty = viaShow

instance Pretty UTCTime where
    pretty = viaShow
    
instance Pretty Day where
    pretty = viaShow

instance (Pretty a,Pretty b) => Pretty (Either a b) where
    pretty = either pretty pretty


