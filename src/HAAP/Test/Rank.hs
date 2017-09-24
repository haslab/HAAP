{-# LANGUAGE RankNTypes, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module HAAP.Test.Rank where

import HAAP.Core
import HAAP.Pretty
import HAAP.Test.Spec
import HAAP.Utils
import HAAP.Web.Hakyll

import Data.Traversable
import Data.List
import Data.Maybe

import qualified Control.Monad.Reader as Reader

class (Eq score,Ord score,Out score) => Score score where
    okScore :: score -> Bool
    appendScores :: [score] -> score

newtype FloatScore = FloatScore { unFloatScore :: Float }
  deriving (Out,Eq,Ord,Show)

instance Score FloatScore where
    okScore (FloatScore x) = x > 0
    appendScores = FloatScore . averageList . map unFloatScore

newtype MaybeFloatScore = MaybeFloatScore { unMaybeFloatScore :: Maybe Float }
  deriving (Eq,Ord,Show)

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

data HaapRank p args db m a score = HaapRank
    { rankPath :: FilePath
    , rankTitle :: String
    , rankIdTag :: String
    , rankHeaders :: Maybe [String]
    , rankTag :: String
    , rankIds :: [a]
    , rankScore :: a -> Haap p args db m [score]
    }

type HaapRankRes a score = [(a,[score],score)]

runHaapRank :: (HaapMonad m,Out a,Score score) => HaapRank p args db m a score -> Haap p args db m (HaapRankRes a score)
runHaapRank rank = do
    scores <- forM (rankIds rank) $ \x -> do
        scores <- rankScore rank x
        return (x,scores,appendScores scores)
    let cmp x y = compare (thr3 y) (thr3 x)
    return $ sortBy cmp scores

data HaapSpecRank p args db m a score = HaapSpecRank
    { sRankPath :: FilePath
    , sRankTitle :: String
    , sRankIdTag :: String
    , sRankTag :: String
    , sRankIds :: [a]
    , sRankSpec :: a -> HaapSpec
    , sRankScore :: HaapTestRes -> Haap p args db m score
    }

runHaapSpecRankWith :: (HaapMonad m,Out a,Score score) => (args -> HaapSpecArgs) -> HaapSpecRank p args db m a score -> Haap p args db m (HaapRankRes a score)
runHaapSpecRankWith getArgs r = runHaapRank (haapSpecRank getArgs r)

haapSpecRank :: HaapMonad m => (args -> HaapSpecArgs) -> HaapSpecRank p args db m a score -> HaapRank p args db m a score
haapSpecRank getArgs r = HaapRank (sRankPath r) (sRankTitle r) (sRankIdTag r) Nothing (sRankTag r) (sRankIds r) rSc
    where
    rSc a = do
        table <- runSpecWith getArgs (sRankSpec r a)
        mapM (sRankScore r . snd) $ haapTestTableRows table


