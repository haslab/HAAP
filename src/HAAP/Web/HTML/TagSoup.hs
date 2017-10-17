module HAAP.Web.HTML.TagSoup where

import HAAP.Core
import HAAP.IO

import Text.HTML.TagSoup

import Data.Default
import Data.Generics

import Debug.Trace

type TagHtml = [Tag String]

parseTagSoupHTML :: HaapMonad m => FilePath -> Haap p args db m TagHtml
parseTagSoupHTML file = do
    str <- runIO $ readFile file
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

injectHTMLHead :: TagHtml -> String -> String
injectHTMLHead newhead = asTagSoupHTML aux
    where
    aux [] = []
    aux (tag@(TagOpen "head" atts):tags) = tag : newhead ++ tags
    aux (x:xs) = x : aux xs