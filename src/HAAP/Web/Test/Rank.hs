{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Rank@ plugin to generate rankings as HTML webpages.
-}

{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module HAAP.Web.Test.Rank where

import HAAP.Core
import HAAP.Pretty
import HAAP.Test.Spec
import HAAP.Test.Rank
import HAAP.Utils
import HAAP.Web.Hakyll
import HAAP.IO
import HAAP.Plugin

import Data.Traversable
import Data.List
import qualified Data.Text as T

import qualified Control.Monad.Reader as Reader
import Control.Monad.IO.Class

renderHaapRank :: (HasPlugin Rank t m,HasPlugin Hakyll t m,Pretty a,Score score) => HaapRank t m a score -> Haap t m FilePath
renderHaapRank rank = do
    scores <- runHaapRank rank
    renderHaapRankScores rank scores

renderHaapSpecRank :: (HasPlugin Spec t m,MonadIO m,HasPlugin Rank t m,HasPlugin Hakyll t m,Pretty a,Score score) => HaapSpecRank t m a score -> Haap t m FilePath
renderHaapSpecRank rank = do
    scores <- runHaapSpecRank rank
    renderHaapRankScores (haapSpecRank rank) scores

renderHaapRankScores :: (HasPlugin Rank t m,HasPlugin Hakyll t m,Pretty a,Score score) => HaapRank t m a score -> HaapRankRes a score -> Haap t m FilePath
renderHaapRankScores rank scores = do
    hp <- getHakyllP
    hakyllRules $ create [fromFilePath $ rankPath rank] $ do
        route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
        compile $ do
            let headerCtx = field "header" (return . itemBody)
            let colCtx = field "col" (return . scoreShow . snd . itemBody)
                       `mappend` field "header" (return . prettyString . fst . itemBody)
                       `mappend` constField "ranktag" (T.unpack $ rankTag rank)
                       `mappend` field "class" (return . scoreClass . snd . itemBody)
            let rowCtx = field "id" (return . prettyString . fst3 . itemBody)
                       `mappend` listFieldWith "cols" colCtx (mapM makeItem . snd3 . itemBody)
                       `mappend` constField "ranktag" (T.unpack $ rankTag rank)
                       `mappend` field "score" (return . scoreShow . Just . thr3 . itemBody)
                       `mappend` field "class" (return . scoreClass . Just . thr3 . itemBody)
            let pageCtx :: Context String
                pageCtx = constField "title" (T.unpack $ rankTitle rank)
                        `mappend` constField "projectpath" (fileToRoot $ hakyllRoute hp $ rankPath rank)
                        `mappend` constField "idtag" (T.unpack $ rankIdTag rank)
                        `mappend` constField "ranktag" (T.unpack $ rankTag rank)
                        `mappend` listField "headers" headerCtx (mapM makeItem headers)
                        `mappend` listField "rows" rowCtx (mapM makeItem scores')
            makeItem "" >>= loadAndApplyHTMLTemplate "templates/ranks.html" pageCtx >>= hakyllCompile hp
    return $ hakyllRoute hp $ rankPath rank
  where
    scores' = map (mapSnd3 (zipLeft headernums)) scores
    numscores [] = 0
    numscores (x:xs) = max (length $ snd3 x) (numscores xs)
    headernums = if numscores scores == 0 then [] else [1..numscores scores]
    headers = case rankHeaders rank of
        Nothing -> map show headernums
        Just xs -> map T.unpack xs
    scoreClass Nothing = "hspec-failure"
    scoreClass (Just x) = if okScore x then "hspec-success" else "hspec-failure"
    scoreShow Nothing = "-"
    scoreShow (Just x) = prettyString x



