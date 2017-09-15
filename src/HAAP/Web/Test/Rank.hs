{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module HAAP.Web.Test.Rank where

import HAAP.Core
import HAAP.Pretty
import HAAP.Test.Spec
import HAAP.Test.Rank
import HAAP.Utils
import HAAP.Web.Hakyll

import Data.Traversable
import Data.List

import qualified Control.Monad.Reader as Reader

renderHaapRank :: (Out a,Score score) => HaapRank p args db a score -> Haap p args db (Rules (),FilePath)
renderHaapRank rank = do
    scores <- runHaapRank rank
    renderHaapRankScores rank scores

renderHaapSpecRankWith :: (Out a,Score score) => (args -> HaapSpecArgs) -> HaapSpecRank p args db a score -> Haap p args db (Rules (),FilePath)
renderHaapSpecRankWith getArgs rank = do
    scores <- runHaapSpecRankWith getArgs rank
    renderHaapRankScores (haapSpecRank getArgs rank) scores

renderHaapRankScores :: (Out a,Score score) => HaapRank p args db a score -> HaapRankRes a score -> Haap p args db (Rules (),FilePath)
renderHaapRankScores rank scores = do
    let rules = create [fromFilePath $ rankPath rank] $ do
        route idRoute
        compile $ do
            let headerCtx = field "header" (return . itemBody)
            let colCtx = field "col" (return . scoreShow . snd . itemBody)
                       `mappend` field "header" (return . pretty . fst . itemBody)
                       `mappend` constField "ranktag" (rankTag rank)
                       `mappend` field "class" (return . scoreClass . snd . itemBody)
            let rowCtx = field "id" (return . pretty . fst3 . itemBody)
                       `mappend` listFieldWith "cols" colCtx (mapM makeItem . snd3 . itemBody)
                       `mappend` constField "ranktag" (rankTag rank)
                       `mappend` field "score" (return . scoreShow . Just . thr3 . itemBody)
                       `mappend` field "class" (return . scoreClass . Just . thr3 . itemBody)
            let pageCtx = constField "title" (rankTitle rank)
                        `mappend` constField "projectpath" (toRoot $ rankPath rank)
                        `mappend` constField "idtag" (rankIdTag rank)
                        `mappend` constField "ranktag" (rankTag rank)
                        `mappend` listField "headers" headerCtx (mapM makeItem headers)
                        `mappend` listField "rows" rowCtx (mapM makeItem scores')
            makeItem "" >>= loadAndApplyTemplate "templates/ranks.html" pageCtx
    return (rules,rankPath rank)
  where
    scores' = map (mapSnd3 (zipLeft headernums)) scores
    numscores [] = 0
    numscores (x:xs) = max (length $ snd3 x) (numscores xs)
    headernums = if numscores scores == 0 then [] else [1..numscores scores]
    headers = case rankHeaders rank of
        Nothing -> map show headernums
        Just xs -> xs
    scoreClass Nothing = "hspec-failure"
    scoreClass (Just x) = if okScore x then "hspec-success" else "hspec-failure"
    scoreShow Nothing = "-"
    scoreShow (Just x) = pretty x


