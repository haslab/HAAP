module HAAP.Web.HTML.Pandoc where

import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.HTML
import Text.Pandoc

import Data.Default
import Data.Generics

import Debug.Trace

asPandocHTML :: (Pandoc -> Pandoc) -> String -> String
asPandocHTML f str = case readHtml def str of
    Left err -> error $ show err
    Right pandoc -> writeHtmlString def (f pandoc)
    
pandocChangeLinkUrls :: (String -> String) -> Pandoc -> Pandoc
pandocChangeLinkUrls furl = everywhere (mkT flink)
    where
    flink :: Inline -> Inline
    flink (Link attrs inlines (url,name)) = Link attrs inlines (furl url,name)
    flink i = i
