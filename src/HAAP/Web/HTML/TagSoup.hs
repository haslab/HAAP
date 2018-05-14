{-
HAAP: Haskell Automated Assessment Platform

This module provides wrappers to the _tagsoup_ library (<https://hackage.haskell.org/package/tagsoup>) for unstructured processing of HTML/XML documents.
-}

{-# LANGUAGE FlexibleContexts #-}

module HAAP.Web.HTML.TagSoup where

import HAAP.Core
import HAAP.IO
import HAAP.Plugin
import HAAP.Core

import Text.HTML.TagSoup

import Data.Default
import Data.Generics

import Debug.Trace

import Control.Monad.IO.Class

type TagHtml = [Tag String]

parseTagSoupHTML :: (HaapStack t m,MonadIO m) => FilePath -> Haap t m TagHtml
parseTagSoupHTML file = do
    str <- runBaseIO' $ readFile file
    return $ parseTags str

asTagSoupHTML :: (TagHtml -> TagHtml) -> String -> String
asTagSoupHTML f str = renderTags $ f $ parseTags str
    
tagSoupChangeLinkUrls :: (String -> String) -> TagHtml -> TagHtml
tagSoupChangeLinkUrls furl = map felem
    where
    felem :: Tag String -> Tag String
    felem (TagOpen x atts) = TagOpen x $ map fatt atts
    felem i = i
    fatt :: Attribute String -> Attribute String
    fatt ("href",url) = ("href",furl url)
    fatt att = att

injectHTMLHead :: TagHtml -> TagHtml -> TagHtml
injectHTMLHead newhead = aux
    where
    aux [] = []
    aux (tag@(TagOpen "head" atts):tags) = tag : newhead ++ tags
    aux (x:xs) = x : aux xs

injectHTMLBody :: TagHtml -> TagHtml -> TagHtml
injectHTMLBody newbody = aux
    where
    aux [] = []
    aux (tag@(TagOpen "body" atts):tags) = tag : newbody ++ tags
    aux (x:xs) = x : aux xs