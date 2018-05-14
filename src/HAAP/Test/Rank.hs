{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Rank@ plugin to generate rankings.
-}

{-# LANGUAGE TypeOperators, DeriveGeneric, UndecidableInstances, FlexibleContexts, EmptyDataDecls, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, DeriveFunctor, DeriveAnyClass, TemplateHaskell, RankNTypes, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module HAAP.Test.Rank where

import HAAP.Core
import HAAP.Pretty
import HAAP.Test.Spec
import HAAP.Utils
import HAAP.Web.Hakyll
import HAAP.IO
import HAAP.Plugin

import Data.Traversable
import Data.List
import Data.Maybe
import Data.SafeCopy
import Data.Default

import Control.Monad.Trans
import Control.Monad.Catch
import Control.Monad.Reader as Reader

data Rank

data RankArgs = RankArgs
    { 
    }
instance Default RankArgs where
    def = RankArgs {}

useRank :: (HaapStack t m,PluginK Rank t m) => Haap (PluginT Rank :..: t) m a -> Haap t m a
useRank m = usePlugin_ (return def) m

instance HaapPlugin Rank where
    type PluginI Rank = RankArgs
    type PluginO Rank = ()
    type PluginT Rank = ReaderT RankArgs
    type PluginK Rank t m = ()

    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . unComposeT) m
        return (x,())

instance HaapMonad m => HasPlugin Rank (ReaderT RankArgs) m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT (ReaderT RankArgs) m (t2 m)) => HasPlugin Rank (ComposeT (ReaderT RankArgs) t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m

class (Eq score,Ord score,Out score) => Score score where
    okScore :: score -> Bool
    appendScores :: [score] -> score

newtype FloatScore = FloatScore { unFloatScore :: Float }
  deriving (Out,Eq,Ord,Show,Generic)
$(deriveSafeCopy 0 'base ''FloatScore)

instance Score FloatScore where
    okScore (FloatScore x) = x > 0
    appendScores = FloatScore . averageList . map unFloatScore

newtype PercentageScore = PercentageScore { unPercentageScore :: Double }
  deriving (Eq,Ord,Show)
$(deriveSafeCopy 0 'base ''PercentageScore)

instance Out PercentageScore where
    docPrec i  = doc
    doc (PercentageScore x) = text (printDouble x 2) <> text "%"

instance Score PercentageScore where
    okScore (PercentageScore x) = x > 50
    appendScores = PercentageScore . averageList . map unPercentageScore

newtype MaybeFloatScore = MaybeFloatScore { unMaybeFloatScore :: Maybe Float }
  deriving (Eq,Ord,Show)
$(deriveSafeCopy 0 'base ''MaybeFloatScore)

instance Score MaybeFloatScore where
    okScore (MaybeFloatScore Nothing) = False
    okScore (MaybeFloatScore (Just x)) = okScore $ FloatScore x
    appendScores = appendScores' . catMaybes . map unMaybeFloatScore
        where
        appendScores' [] = MaybeFloatScore $ Nothing
        appendScores' xs = MaybeFloatScore $ Just $ unFloatScore $ appendScores $ map FloatScore xs

instance Out MaybeFloatScore where
    docPrec i x = doc x
    doc (MaybeFloatScore Nothing) = text "-"
    doc (MaybeFloatScore (Just x)) = doc x

data HaapRank t m a score = HaapRank
    { rankPath :: FilePath
    , rankTitle :: String
    , rankIdTag :: String
    , rankHeaders :: Maybe [String]
    , rankTag :: String
    , rankIds :: [a]
    , rankScore :: a -> Haap t m [score]
    }

type HaapRankRes a score = [(a,[score],score)]

runHaapRank :: (HasPlugin Rank t m,Out a,Score score) => HaapRank t m a score -> Haap t m (HaapRankRes a score)
runHaapRank rank = do
    scores <- forM (rankIds rank) $ \x -> do
        scores <- rankScore rank x
        return (x,scores,appendScores scores)
    let cmp x y = compare (thr3 y) (thr3 x)
    return $ sortBy cmp scores

data HaapSpecRank t m a score = HaapSpecRank
    { sRankPath :: FilePath
    , sRankTitle :: String
    , sRankIdTag :: String
    , sRankTag :: String
    , sRankIds :: [a]
    , sRankSpec :: a -> HaapSpec
    , sRankScore :: HaapTestRes -> Haap t m score
    }

runHaapSpecRank :: (MonadIO m,HasPlugin Rank t m,HasPlugin Spec t m,Out a,Score score) => HaapSpecRank t m a score -> Haap t m (HaapRankRes a score)
runHaapSpecRank r = runHaapRank (haapSpecRank r)

haapSpecRank :: (MonadIO m,HasPlugin Spec t m,HasPlugin Rank t m) => HaapSpecRank t m a score -> HaapRank t m a score
haapSpecRank r = HaapRank (sRankPath r) (sRankTitle r) (sRankIdTag r) Nothing (sRankTag r) (sRankIds r) rSc
    where
    rSc a = do
        table <- runSpec (sRankSpec r a)
        mapM (sRankScore r . snd) $ haapTestTableRows table


