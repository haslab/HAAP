{-
HAAP: Haskell Automated Assessment Platform

This module provides wrappers to the _pandoc_ library (<https://hackage.haskell.org/package/pandoc>) for inter-format markdown processing.
-}

module HAAP.Web.HTML.Pandoc where

import HAAP.Pretty

import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.HTML
import Text.Pandoc
import Text.Parsec.Error

import Data.Default
import Data.Generics

import Debug.Trace

instance Out PandocError where
    docPrec i x = doc x
    doc (ParseFailure str) = text "parsing failure:" <+> text str
    doc (ParsecError _ err) = text "parsing failure:" <+> doc err

instance Out ParseError where
    docPrec i x = doc x
    doc e = text "parsec failed at" <+> text (show $ errorPos e) <> char ':' $+$ nest 4 (vcat $ map doc msgs)
        where msgs = errorMessages e

instance Out Message where
    docPrec i x = doc x
    doc (SysUnExpect x) = text "system unexpected:" <+> text x
    doc (UnExpect x) = text "unexpected:" <+> text x
    doc (Expect x) = text "expected:" <+> text x
    doc (Message x) = text x

asPandocHTML :: (Pandoc -> Pandoc) -> String -> String
asPandocHTML f str = case readHtml def str of
    Left err -> error $ pretty err
    Right pandoc -> writeHtmlString def (f pandoc)
    
pandocChangeLinkUrls :: (String -> String) -> Pandoc -> Pandoc
pandocChangeLinkUrls furl = everywhere (mkT flink)
    where
    flink :: Inline -> Inline
    flink (Link attrs inlines (url,name)) = Link attrs inlines (furl url,name)
    flink i = i
