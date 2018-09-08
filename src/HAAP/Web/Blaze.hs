{-
HAAP: Haskell Automated Assessment Platform

This module provides wrappers to the _blaze-html_ library (<https://hackage.haskell.org/package/blaze-html>) for HTML generation.
-}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module HAAP.Web.Blaze
    ( module HAAP.Web.Blaze
    , module Text.Blaze.Html5
    , module Text.Blaze.Html.Renderer.Text
    ) where

import HAAP.Pretty

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!),toHtml,Html,docTypeHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy as TL

instance Pretty Html where
    pretty x = text $ TL.toStrict $ renderHtml x