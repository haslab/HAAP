{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module HAAP.Web.Blaze
    ( module HAAP.Web.Blaze
    , module Text.Blaze.Html5
    , module Text.Blaze.Html.Renderer.String
    ) where

import HAAP.Pretty

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!),toHtml,Html,docTypeHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String

instance Out Html where
    docPrec i x = doc x
    doc x = text $ renderHtml x