{-# LANGUAGE CPP #-}

{-
HAAP: Haskell Automated Assessment Platform

This module provides wrappers to the _pandoc_ library (<https://hackage.haskell.org/package/pandoc>) for inter-format markdown processing.
-}

module HAAP.Web.HTML.Pandoc where

import HAAP.Pretty as PP

import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.HTML
import Text.Pandoc
import Text.Parsec.Error

import Data.Default
import Data.Generics
import qualified Data.Text as T

--import Debug.Trace

instance Pretty PandocError where
    pretty = string . show
    --doc (ParseFailure str) = text "parsing failure:" <+> text str
    --doc (ParsecError _ err) = text "parsing failure:" <+> doc err

instance Pretty ParseError where
    pretty e = string "parsec failed at" <+> string (show $ errorPos e) <> char ':' $+$ nest 4 (vcat $ map pretty msgs)
        where msgs = errorMessages e

instance Pretty Message where
    pretty (SysUnExpect x) = string "system unexpected:" <+> string x
    pretty (UnExpect x) = string "unexpected:" <+> string x
    pretty (Expect x) = string "expected:" <+> string x
    pretty (Message x) = string x

asPandocHTML :: (Pandoc -> Pandoc) -> String -> String
#if MIN_VERSION_pandoc (2,0,0)
asPandocHTML f str = case runPure (readHtml def (T.pack str)) of
    Left err -> error $ prettyString err
    Right pandoc -> case runPure (writeHtml5String def (f pandoc)) of
        Left err -> error $ prettyString err
        Right txt -> T.unpack txt
#else
asPandocHTML f str = case readHtml def str of
    Left err -> error $ pretty err
    Right pandoc -> writeHtmlString def (f pandoc) 
#endif
    
pandocChangeLinkUrls :: (String -> String) -> Pandoc -> Pandoc
pandocChangeLinkUrls furl = everywhere (mkT flink)
    where
    flink :: Inline -> Inline
    flink (Link attrs inlines (url,name)) = Link attrs inlines (furl url,name)
    flink i = i
