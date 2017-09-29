module HAAP.Web.HTML.TagSoup where

import Text.HTML.TagSoup

import Data.Default
import Data.Generics

import Debug.Trace

type TagHtml = [Tag String]

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
