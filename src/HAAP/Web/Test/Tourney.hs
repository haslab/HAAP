{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

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

import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List
import Data.String
import Data.Time.LocalTime
import Data.Traversable
import Data.Foldable

import Control.Monad

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import System.FilePath

newtype ScoredTourneyPlayer a r = ScoredTourneyPlayer (a,r)
  deriving (Eq,Ord,Show)

instance Out a => Out (ScoredTourneyPlayer a r) where
    docPrec i x = doc x
    doc (ScoredTourneyPlayer x) = doc $ fst x

renderHaapTourney :: (HaapDB db,TourneyPlayer a) => HaapTourney p args db a r -> Haap p args (DB db) (Rules ())
renderHaapTourney tourney = do
    (tourneyno,tourneydb,tourneyTree,tourneyTime) <- runHaapTourney tourney
    let db = tourneyDB tourneydb
    web1 <- renderHaapTourneyDB tourney db
    web2 <- renderHaapTourneyTree tourney tourneyno tourneyTree tourneyTime
    return $ web1 >> web2

renderHaapTourneyDB :: (TourneyPlayer a) => HaapTourney p args db a r -> [(Int,HaapTourneySt a)] -> Haap p args (DB db) (Rules ())
renderHaapTourneyDB t db = do
    --runIO $ putStrLn $ "render " ++ show (toRoot $ tourneysPath)
    renderHaapRank rank
  where
    tourneysPath = tourneyPath t </> addExtension "tourneys" "html"
    makeTourneyPath tourneyno = "tourneys" </> addExtension ("tourney" ++ pretty tourneyno) "html"
    makeHeader i = pretty $ H.a ! A.href (fromString $ makeTourneyPath i) $ H.preEscapedToMarkup i
    headers = map (makeHeader) (map fst db)
    rank = HaapRank
        tourneysPath
        (tourneyTitle t)
        (tourneyPlayerTag t)
        (Just headers)
        "Classificação Média"
        ranks
        score
    score (ScoredTourneyPlayer (player,scores)) = return scores
    
--    rankByGroup :: Map a Int -> Map a [Maybe Float]
    rankByGroup = Map.map (\f -> ([Just $ realToFrac f]))
    joinRanks k (fs1) (fs2) = Just (fs1++fs2)
    leftRanks = Map.map (\(fs) -> (fs++[Nothing]))
    rightRanks = Map.map (\(fs) -> (Nothing:fs))
--    ranksByGroup :: Map a [Maybe Float]
    ranksByGroup = foldr0 (Map.mergeWithKey joinRanks leftRanks rightRanks) (map (rankByGroup . snd) db) Map.empty
    cmp (_,s1) (_,s2) = compare (getRank s1) (getRank s2)
    getRank :: [Maybe Float] -> Float
    getRank = averageList . catMaybes
--    ranks :: [(a,[MaybeFloatScore])]
    ranks = map (ScoredTourneyPlayer . mapSnd (map MaybeFloatScore)) $ sortBy cmp $ Map.toList ranksByGroup
    
renderHaapTourneyTree :: (TourneyPlayer a) => HaapTourney p args db a r -> Int -> TourneyTree a r -> ZonedTime -> Haap p args (DB db) (Rules ())
renderHaapTourneyTree t no tree time = do
    let tPath =  tourneyPath t </> "tourneys" </> addExtension ("tourney" ++ pretty no) "html"
    size <- getTourneySize $ tourneyPlayers t
    let title = "Tourney " ++ pretty no ++ " " ++ show time
    let header = H.h1 $ fromString title
    let csspath = toRoot tPath </> "css"
    
    return $ create [fromFilePath tPath] $ do
        route idRoute
        tree' <- mapM (mapM (mapSndM $ renderMatch t)) tree
        compile $ do
            makeItem $ pretty $ tourneyHTML csspath tree' size title header

tourneyHTML :: TourneyPlayer a => FilePath -> TourneyTree a [Link] -> Int -> String -> Html -> Html
tourneyHTML pathtocss (r:rs) tsize title header = docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title $ fromString title
        H.link ! A.rel "stylesheet" ! A.href (fromString $ pathtocss </> "tourney.css")
    H.body $ do
        header
        roundsHTML tsize Nothing (r:rs)

roundsHTML :: TourneyPlayer a => Int -> Maybe Int -> [Round a [Link]] -> Html
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

roundHTML :: TourneyPlayer a => Maybe Int -> Round a [Link] -> Html
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

matchHTML :: TourneyPlayer a => Int -> Bool -> Match a [Link] -> Html
matchHTML round isFst (players,links) = do
    let winplayers = zip players $ replicate (roundWinners round) True ++ repeat False
    let first = if isFst then "-first" else ""
    H.div ! A.class_ (fromString $ "bracket-game-"++show round++first) $ do
        mapM_ (\(i,(p,w)) -> playerHMTL i p w) $ zip [1..] winplayers 
    H.div ! A.id "battle" $ forM_ links $ \link -> do
        H.preEscapedToHtml $ ("&nbsp;"::String)
        H.a ! A.href (fromString link) $ "vs"
 
playerHMTL :: TourneyPlayer a => Int -> a -> Bool -> Html
playerHMTL i p win = do
    let wintag = if isDefaultPlayer p then "bot" else if win then "win" else "loss"
    H.div ! A.class_ (fromString $ "player top "++wintag) $ do
        H.preEscapedToMarkup (pretty p)
        H.div ! A.class_ "score" $ H.preEscapedToMarkup i




