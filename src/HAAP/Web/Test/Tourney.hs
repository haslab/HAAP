{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Tourney@ plugin to run tournaments and generate HTML webpages with the brackets.
-}


{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TemplateHaskell, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module HAAP.Web.Test.Tourney where

import HAAP.Core
import HAAP.DB
import HAAP.Test.Tourney
import HAAP.Test.Rank
import HAAP.Web.Hakyll
import HAAP.Web.Blaze
import HAAP.Web.Test.Rank
import HAAP.Utils
import HAAP.Pretty
import HAAP.IO
import HAAP.Plugin

import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List
import Data.String
import Data.Time.LocalTime
import Data.Traversable
import Data.Foldable
import Data.SafeCopy
import Data.Proxy
import qualified Data.Text as T

import Control.Monad
import Control.DeepSeq
import Control.Monad.IO.Class

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import System.FilePath

newtype ScoredTourneyPlayer a r = ScoredTourneyPlayer (a,r)
  deriving (Eq,Ord,Show)

instance Pretty a => Pretty (ScoredTourneyPlayer a r) where
    pretty (ScoredTourneyPlayer x) = pretty $ fst x

renderHaapTourney :: (PluginK db t m,MonadIO m,HasPlugin Rank t m,HasDB db t m,HasPlugin Hakyll t m,HasPlugin Tourney t m,NFData a,HaapDB db,TourneyPlayer a) => HaapTourney t m db a r -> Haap t m FilePath
renderHaapTourney tourney = do
    (tourneyno,tourneydb,tourneyTree,tourneyTime) <- runHaapTourney tourney
    let db = tourneyDB tourneydb
    index <- renderHaapTourneyDB tourney db
    renderHaapTourneyTree tourney tourneyno tourneyTree tourneyTime
    return index

renderHaapTourneyDB :: (HasPlugin Rank t m,HasDB db t m,HasPlugin Hakyll t m,HasPlugin Tourney t m,TourneyPlayer a) => HaapTourney t m db a r -> [(Int,HaapTourneySt a)] -> Haap t m FilePath
renderHaapTourneyDB t db = do
    hp <- getHakyllP
    --runIO $ putStrLn $ "render " ++ show (toRoot $ tourneysPath)
    let tourneysPath = tourneyPath t </> addExtension "tourneys" "html"
        makeTourneyPath tourneyno = hakyllRoute hp $ addExtension ("tourney" ++ prettyString tourneyno) "html"
        makeHeader i = prettyText $ H.a ! A.href (fromString $ makeTourneyPath i) $ H.preEscapedToMarkup i
        headers = map (makeHeader) (map fst db)
        rank = HaapRank
                    tourneysPath
                    (tourneyTitle t)
                    (tourneyNotes t)
                    (tourneyPlayerTag t)
                    (Just headers)
                    "Classificação Média"
                    ranks
                    score
        score (ScoredTourneyPlayer (player,scores)) = return scores
        
        --nkByGroup :: Map a Int -> Map a [Maybe Float]
        rankByGroup = Map.map (\f -> ([Just $ realToFrac f]))
        joinRanks k (fs1) (fs2) = Just (fs1++fs2)
        leftRanks = Map.map (\(fs) -> (fs++[Nothing]))
        rightRanks = Map.map (\(fs) -> (Nothing:fs))
        --nksByGroup :: Map a [Maybe Float]
        ranksByGroup = foldr0 (Map.mergeWithKey joinRanks leftRanks rightRanks) (map (rankByGroup . snd) db) Map.empty
        getRank = averageList . catMaybes
        cmp (_,s1) (_,s2) = compare (getRank s2) (getRank s1)
--      t getRank :: [Maybe Float] -> Float
        --nks :: [(a,[MaybeFloatScore])]
        ranks = map (ScoredTourneyPlayer . mapSnd (map MaybeFloatRank)) $ sortBy cmp $ Map.toList ranksByGroup
    renderHaapRank rank
    
renderHaapTourneyTree :: (HasDB db t m,HasPlugin Hakyll t m,HasPlugin Tourney t m,TourneyPlayer a) => HaapTourney t m db a r -> Int -> TourneyTree a r -> ZonedTime -> Haap t m FilePath
renderHaapTourneyTree (t::HaapTourney t m db a r) no tree time = do
    hp <- getHakyllP
    let tPath =  tourneyPath t </> addExtension ("tourney" ++ prettyString no) "html"
    size <- case tourneyPlayers t of
        Left ps -> getTourneySize (Proxy::Proxy db) ps
        Right ps -> return $ length $ concat ps
    let title = "Tourney " <> prettyText no <> " " <> prettyText time
    let header = H.h1 $ H.preEscapedToMarkup title
    let csspath = fileToRoot tPath </> "css"
    
    hakyllRules $ create [fromFilePath tPath] $ do
        route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
        tree' <- mapM (mapM (mapSndM (mapM (renderMatch t)))) tree
        compile $ do
            makeItem (prettyString $ tourneyHTML csspath tree' size title header) >>= hakyllCompile hp
    return $ hakyllRoute hp tPath

tourneyHTML :: TourneyPlayer a => FilePath -> TourneyTree a Link -> Int -> T.Text -> Html -> Html
tourneyHTML pathtocss (r:rs) tsize title header = docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title $ H.preEscapedToMarkup title
        H.link ! A.rel "stylesheet" ! A.href (fromString $ pathtocss </> "tourney.css")
    H.body $ do
        let bigsmall = if tsize >= 64 then "big" else "small"
        H.div ! A.class_ (fromString bigsmall) $ do
            header
            roundsHTML tsize Nothing (r:rs)

roundsHTML :: TourneyPlayer a => Int -> Maybe Int -> [Round a Link] -> Html
roundsHTML fstround _ [] = return ()
roundsHTML fstround prevround (r:rs) = do
    roundHTML prevround r
    let prevround' = Just $ maybe fstround nextRound prevround
    roundsHTML fstround prevround' rs

roundOf :: Maybe Int -> Int -> String
roundOf Nothing r = "r-of-"++show r
roundOf (Just prev) r = "r-of-"++show prev++"-"++show r

roundPat :: Int -> [Bool]
roundPat 128 = concat $ repeat [True,False]
roundPat n = True : repeat False

roundHTML :: TourneyPlayer a => Maybe Int -> Round a Link -> Html
roundHTML prevround (m:ms) = do
    let round = length (m:ms) * 4
    connectorsHTML prevround round
    H.div ! A.class_ (fromString $ "round "++roundOf prevround round) $ do
        mapM_ (\(isFst,m) -> matchHTML round isFst m) $ zip (roundPat round) (m:ms)
    when (round==4) $ do
        H.div ! A.class_ "round r-of-4-1" $ do
            H.div ! A.class_ "connectors r-of-4-1" $ do
                H.div ! A.class_ "next-line" $ return ()
            H.div !A.class_ "bracket-game-1-first" $ do
                playerHMTL 1 (Prelude.head $ fst $ last (m:ms)) True

connectorsHTML :: Maybe Int -> Int -> Html
connectorsHTML Nothing round = return ()
connectorsHTML (Just prevround) round = do
    let len = round `Prelude.div` 4
    H.div ! A.class_ (fromString $ "connectors "++roundOf (Just prevround) round) $ do
        connectorHTML prevround round
        let space = H.div ! A.class_ (fromString $ "space-"++show round) $ return ()
        replicateM_ (len-1) $ space >> connectorHTML prevround round

connectorHTML :: Int -> Int -> Html
connectorHTML prevround round = do                
    H.div ! A.class_ "top-line"     $ return ()
    H.div ! A.class_ "clear"        $ return ()
    H.div ! A.class_ "mid-line1"    $ return ()
    H.div ! A.class_ "clear"        $ return ()
    H.div ! A.class_ "mid-line2"   $ return ()
    H.div ! A.class_ "clear"        $ return ()
    H.div ! A.class_ "bottom-line"  $ return ()
    H.div ! A.class_ "clear"        $ return ()
    H.div ! A.class_ "vert-line"    $ return ()
    H.div ! A.class_ "clear"        $ return ()
    H.div ! A.class_ "next-line"    $ return ()
    H.div ! A.class_ "clear"        $ return ()

matchHTML :: TourneyPlayer a => Int -> Bool -> Match a Link -> Html
matchHTML round isFst (players,links) = do
    let winplayers = zip players $ replicate (roundWinners round) True ++ repeat False
    let first = if isFst then "-first" else ""
    H.div ! A.class_ (fromString $ "bracket-game-"++show round++first) $ do
        mapM_ (\(i,(p,w)) -> playerHMTL i p w) $ zip [1..] winplayers 
    H.div ! A.id "battle" $ forM_ (links) $ \link -> do
        H.preEscapedToHtml $ ("&nbsp;"::String)
        H.a ! A.href (fromString link) $ "vs"
 
playerHMTL :: TourneyPlayer a => Int -> a -> Bool -> Html
playerHMTL i p win = do
    let wintag = if isDefaultPlayer p then "bot" else if win then "win" else "loss"
    H.div ! A.class_ (fromString $ "player top "++wintag) $ do
        renderPlayer p
        H.div ! A.class_ "score" $ H.preEscapedToMarkup i


newtype MaybeFloatRank = MaybeFloatRank { unMaybeFloatRank :: Maybe Float }
  deriving (Eq,Show)
$(deriveSafeCopy 0 'base ''MaybeFloatRank)

instance Ord MaybeFloatRank where
    compare (MaybeFloatRank x) (MaybeFloatRank y) = compare y x

instance Score MaybeFloatRank where
    okScore (MaybeFloatRank Nothing) = False
    okScore (MaybeFloatRank (Just x)) = okScore $ FloatScore x
    appendScores = appendScores' . catMaybes . map unMaybeFloatRank
        where
        appendScores' [] = MaybeFloatRank $ Nothing
        appendScores' xs = MaybeFloatRank $ Just $ unFloatScore $ appendScores $ map FloatScore xs

instance Pretty MaybeFloatRank where
    pretty (MaybeFloatRank Nothing) = text "-"
    pretty (MaybeFloatRank (Just x)) = pretty x

