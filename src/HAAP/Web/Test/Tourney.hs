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

import Safe

newtype ScoredTourneyPlayer a r = ScoredTourneyPlayer (a,r)
  deriving (Eq,Ord,Show)

instance Pretty a => Pretty (ScoredTourneyPlayer a r) where
    pretty (ScoredTourneyPlayer x) = pretty $ fst x

renderHaapTourney :: (Show a,PluginK db t m,MonadIO m,HasPlugin Rank t m,HasDB db t m,HasPlugin Hakyll t m,HasPlugin Tourney t m,NFData a,HaapDB db,TourneyPlayer a) => HaapTourney t m db a r -> Haap t m FilePath
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
    nplayers <- case tourneyPlayers t of
        Left ps -> return $ length ps --getTourneySize (Proxy::Proxy db) ps
        Right ps -> return $ length $ concat ps
    let rounds = tourneyRounds t nplayers
    let title = "Tourney " <> prettyText no <> " " <> prettyText time
    let header = H.h1 $ H.preEscapedToMarkup title
    let csspath = fileToRoot tPath </> "css"
    
    let cssCtx   = listField "rounds" roundCtx (mapM makeItem $ tourneyLayout rounds)
        roundCtx = field "roundno"      (return . prettyString . fst4 . itemBody)
         `mappend` field "matchHeight"  (return . prettyString . snd4 . itemBody)
         `mappend` field "mergerHeight" (return . prettyString . thr4 . itemBody)
         `mappend` field "marginHeight" (return . prettyString . fou4 . itemBody)
    
    hakyllRules $ do
        create [fromFilePath tPath] $ do
            route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
            tree' <- mapM (mapM (mapM (mapSndM (mapM (renderMatch t))))) tree
            compile $ do
                makeItem (prettyString $ tourneyHTML no csspath tree' rounds title header) >>= hakyllCompile hp
        create [fromFilePath $ "css/tourney"++show no++".css"] $ do
            route $ idRoute `composeRoutes` funRoute (hakyllRoute hp)
            compile $ do
                makeItem "" >>= loadAndApplyHTMLTemplate "templates/tourney.css" cssCtx >>= hakyllCompile hp
    return $ hakyllRoute hp tPath

tourneyLayout :: [TourneyRound] -> [(Int,Int,Int,Int)]
tourneyLayout rs = tourneyLayout' Nothing (zip [1..] rs)

tourneyLayout' :: Maybe (TourneyRound,(Int,Int,Int,Int)) -> [(Int,TourneyRound)] -> [(Int,Int,Int,Int)]
tourneyLayout' prev [] = []
tourneyLayout' prev ((rno,r):rs) = prevLayout : tourneyLayout' (Just (r,prevLayout)) rs
    where
    prevLayout = roundLayout rno prev r (fmap snd $ headMay rs)

roundLayout :: Int -> Maybe (TourneyRound,(Int,Int,Int,Int)) -> TourneyRound -> Maybe TourneyRound -> (Int,Int,Int,Int)
roundLayout rno r0 r Nothing = (rno,matchHeight,0,0)
    where
    nmatches = divNote "nmatches" (tourneyRoundSize r) (tourneyRoundMatchSize r)
    matchHeight = 2 * tourneyRoundMatchSize r
roundLayout rno Nothing r (Just r1) = (rno,matchHeight,mergerHeight,2)
    where
    nmatches = divNote "nmatches" (tourneyRoundSize r) (tourneyRoundMatchSize r)
    matchHeight = 2 * tourneyRoundMatchSize r
    nmatches1 = divNote "nmatches1" (tourneyRoundSize r1) (tourneyRoundMatchSize r1)
    matchHeight1 = 2 * tourneyRoundMatchSize r1
    nwinners = divNote "nwinners" nmatches nmatches1
    mergerHeight = nwinners * matchHeight + (pred nwinners) * 2
roundLayout rno (Just (r0,(rno0,matchHeight0,mergerHeight0,marginHeight0))) r (Just r1) = (rno,matchHeight,mergerHeight,marginHeight)
    where
    nmatches0 = divNote "nmatches0" (tourneyRoundSize r0) (tourneyRoundMatchSize r0)
    matchHeight0 = 2 * tourneyRoundMatchSize r0
    nmatches = divNote "nmatches" (tourneyRoundSize r) (tourneyRoundMatchSize r)
    matchHeight = 2 * tourneyRoundMatchSize r
    nmatches1 = divNote "nmatches1" (tourneyRoundSize r1) (tourneyRoundMatchSize r1)
    matchHeight1 = 2 * tourneyRoundMatchSize r1
    nwinners0 = divNote "nwinners0" nmatches0 nmatches
    nwinners = divNote "nwinners" nmatches nmatches1
    mergerHeight = nwinners * matchHeight + (pred nwinners) * marginHeight
    
    nmarginblocks = divNote "nmarginblocks" (nmatches0 - (pred nwinners0) - nmatches) (pred nmatches)
    marginHeight = nmarginblocks * matchHeight + (succ nmarginblocks) * marginHeight0

tourneyHTML :: TourneyPlayer a => Int -> FilePath -> TourneyTree a Link -> [TourneyRound] -> T.Text -> Html -> Html
tourneyHTML no pathtocss rs rounds title header = docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title $ H.preEscapedToMarkup title
        H.link ! A.rel "stylesheet" ! A.href (fromString $ pathtocss </> "normalize.css")
        H.link ! A.rel "stylesheet" ! A.href (fromString $ pathtocss </> ("tourney"++show no++".css"))
    H.body $ do
        header
        H.div ! A.class_ "bracket" $ do
            roundsHTML (zip3 [1..] rs rounds)

roundsHTML :: TourneyPlayer a => [(Int,Round a Link,TourneyRound)] -> Html
roundsHTML [] = return ()
roundsHTML ((rno,r,round_):rs) = do
    roundHTML rno r round_ (null rs)
    roundsHTML rs
    
--groupMatches :: [Match a r] -> Maybe TourneyRound -> [[Match a r]]
--groupMatches ms Nothing = [[ms]]
--groupMatches ms (Just nextRound) = groupN (tourneyRoundMatchSize nextRound)

--roundOf :: Maybe Int -> Int -> String
--roundOf Nothing r = "r-of-"++show r
--roundOf (Just prev) r = "r-of-"++show prev++"-"++show r

--roundPat :: Int -> [Bool]
--roundPat 128 = concat $ repeat [True,False]
--roundPat n = True : repeat False

roundHTML :: TourneyPlayer a => Int -> Round a Link -> TourneyRound -> Bool -> Html
roundHTML rno ws round_ isLastRound = do
--    let round = length (m:ms) * 4
--    connectorsHTML prevround round
    H.section ! A.class_ (fromString $ "round " ++ "round" ++ show rno) $ do
        forM_ ws $ \w -> winnersHTML w round_ isLastRound
 
winnersHTML :: TourneyPlayer a => [Match a Link] -> TourneyRound -> Bool -> Html
winnersHTML ms round_ isLastRound = do
    H.div ! A.class_ "winners" $ do
        H.div ! A.class_ "matchups" $ do
            forM_ ms $ \m -> matchupHTML m round_
        unless isLastRound $ H.div ! A.class_ "connector" $ do
            H.div ! A.class_ "merger" $ return ()

matchupHTML :: TourneyPlayer a => Match a Link -> TourneyRound -> Html
matchupHTML (players,links) round_ = do
    H.div ! A.class_ "matchup" $ do
        H.div ! A.class_ "result" $ linksHTML links
        H.div ! A.class_ "participants" $ do
            let nwins = tourneyRoundMatchWinners round_
            forM_ players $ \player -> playerHTML player round_
            
linksHTML :: [Link] -> Html
linksHTML [] = return ()
linksHTML [l] = linkHTML l
linksHTML (l:ls) = do
    linkHTML l
    H.preEscapedToHtml $ ("&nbsp;"::String)
    linksHTML ls

linkHTML :: Link -> Html
linkHTML link = H.a ! A.href (fromString link) $ "vs"

playerHTML :: TourneyPlayer a => ((a,Int),Bool) -> TourneyRound -> Html
playerHTML ((p,ppos),pwin) round_ = do
    let wintag = if isDefaultPlayer p then "" else if pwin then "winner" else "loser"
    H.div ! A.class_ (fromString $ "participant " ++ wintag) $ do
        H.span $ renderPlayer p
        H.span ! A.class_ "score" $ H.preEscapedToMarkup ppos

--matchHTML :: TourneyPlayer a => Int -> Bool -> Match a Link -> Html
--matchHTML round isFst (players,links) = do
--    let winplayers = zip players $ replicate (roundWinners round) True ++ repeat False
--    let first = if isFst then "-first" else ""
--    H.div ! A.class_ (fromString $ "bracket-game-"++show round++first) $ do
--        mapM_ (\(i,(p,w)) -> playerHMTL i p w) $ zip [1..] winplayers 
--    H.div ! A.id "battle" $ forM_ (links) $ \link -> do
--        H.preEscapedToHtml $ ("&nbsp;"::String)
--        H.a ! A.href (fromString link) $ "vs"
 
--playerHMTL :: TourneyPlayer a => Int -> a -> Bool -> Html
--playerHMTL i p win = do
--    let wintag = if isDefaultPlayer p then "bot" else if win then "win" else "loss"
--    H.div ! A.class_ (fromString $ "player top "++wintag) $ do
--        renderPlayer p
--        H.div ! A.class_ "score" $ H.preEscapedToMarkup i


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

