{-# LANGUAGE EmptyDataDecls, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import HAAP

import Data.Default
import Data.Binary
import Data.Char
import Data.Monoid hiding ((<>))
import qualified Data.Map as Map

import Control.DeepSeq
import Control.Monad.IO.Class

import System.Random.Shuffle
import System.Process

import GHC.Generics (Generic(..))

example :: Project
example = Project
    { projectName = "minimalistic"
    , projectPath = "."
    , projectTmpPath = "tmp"
    , projectGroups = []
    , projectTasks = []
    }

emptyLi1DB = Example_DB (HaapTourneyDB 1 [])

exSpec :: HaapSpec
exSpec = bounded "x" [97,98,3,4] $ \x ->
          bounded "y" "abcd" $ \y -> 
          testEqual x (ord y)
          
exRankSpec :: Int -> HaapSpec
exRankSpec i = bounded "x" [1,2,3,4,5] $ \x -> testEqual x i

data Example_DB = Example_DB
    { exTourneyDB :: HaapTourneyDB ExPlayer
    }
  deriving (Generic)
instance Binary Example_DB

lnsTourney :: DBLens (BinaryDB Example_DB) (HaapTourneyDB ExPlayer)
lnsTourney = DBLens
    (BinaryDBQuery exTourneyDB)
    (\st -> BinaryDBUpdate $ \db -> ((),db { exTourneyDB = st }) )
    
exRankScore :: HaapTestRes -> FloatScore
exRankScore HaapTestOk = FloatScore 1
exRankScore _ = FloatScore 0

exRank :: HaapStack t m => HaapSpecRank t m Int FloatScore
exRank = HaapSpecRank "ranks.html" "Ranks" "Group" "Ranking" [1..10::Int] (exRankSpec) (return . exRankScore)

main = do
    let cfg = HakyllArgs defaultConfiguration False True def
    let exDBArgs = BinaryDBArgs "db" emptyLi1DB def
    let specArgs = HaapSpecArgs HaapSpecQuickCheck Nothing def
    runHaap example $ useHakyll cfg $ useBinaryDB exDBArgs $ do
        (spec,rank,tour) <- useSpec specArgs $ do
            spec <- renderHaapSpecs "spec.html" "Spec" [("spec1",exSpec)]
            (rank,tour) <- useRank $ do
                rank <- renderHaapSpecRank exRank
                tour <- useTourney $ renderHaapTourney exTourney
                return (rank,tour)
            return (spec,rank,tour)
        
        hadcks <- useAndRunHaddock exHaddock
        
        hlint <- useAndRunHLint exHLint
        
        homplexity <- useAndRunHomplexity exHomplexity
        
        (_,hpc) <- useAndRunHpc exHpc () $ const $ do
            runBaseIO $ system "./HPCTest < ints"
            runBaseIO $ system "./HPCTest < ints"
            return ()
            
        cw1 <- useAndRunCodeWorld exCodeWorldDraw
        cw2 <- useAndRunCodeWorld exCodeWorldGame
        
        hakyllRules $ do
            match (fromGlob ("*.png")) $ do
                route idRoute
                compile copyFileCompiler
            match (fromGlob ("*.jpg")) $ do
                route idRoute
                compile copyFileCompiler
            match (fromGlob ("templates/minimalistic.html")) $ do
                route idRoute
                compile templateBodyCompiler
            create ["index.html"] $ do
                route idRoute
                compile $ do
                    let exCtx = constField "projectpath" "."
                              `mappend` constField "spec" spec
                              `mappend` constField "rank" rank
                              `mappend` constField "tour" tour
                              `mappend` listField "haddocks" (field "haddock" (return . itemBody)) (mapM makeItem hadcks)
                              `mappend` constField "hlint" hlint
                              `mappend` constField "homplexity" homplexity
                              `mappend` constField "hpc" hpc
                              `mappend` listField "cws" (field "cw" (return . itemBody)) (mapM makeItem [cw1,cw2])
                    makeItem "" >>= loadAndApplyTemplate "templates/minimalistic.html" exCtx
        
        return ()

data ExPlayer = ExPlayer (String,Bool)
 deriving (Eq,Ord,Show,Generic)
instance Binary ExPlayer
instance NFData ExPlayer

instance Out ExPlayer where
    docPrec i x = doc x
    doc (ExPlayer x) = text (fst x)

instance TourneyPlayer ExPlayer where
    isDefaultPlayer (ExPlayer (_,b)) = b
    defaultPlayer = return $ ExPlayer ("random",True)

exTourney :: MonadIO m => HaapTourney t m (BinaryDB Example_DB) ExPlayer Link
exTourney = HaapTourney 10 "Tourney" bestof "Group" grupos "torneio" lnsTourney match (return) (const $ return ())
    where
    bestof = const 1
    grupos = Left $ map (ExPlayer . mapFst show) $ zip [1..] (replicate 90 False ++ replicate 10 True)
    match tno rno mno players = do
        players' <- runBaseIO' $ shuffleM players
        return (zip players' [1..],"link")



exHaddock = HaddockArgs (Sandbox Nothing) "mininalistic" [] "." ["minimalistic.hs"] "doc"
exHLint = HLintArgs (Sandbox Nothing) [] "." ["minimalistic.hs"] "hlint.html"
exHomplexity = HomplexityArgs (Sandbox Nothing) [] "." ["../../src/"] "homplexity.html"

exHpc :: HpcArgs
exHpc = HpcArgs "HPCTest" def def (Just "hpc") False

exCodeWorldDraw :: CodeWorldArgs
exCodeWorldDraw = CodeWorldArgs (Left "MMDraw.hs") "Draw" (CWDraw CWDrawButton "[Avanca,Avanca]") ghcjs def "codeworld" imgs []
    where
    ghcjs = def { ghcjsSafe = False }
--    db = ["../.cabal-sandbox/x86_64-osx-ghcjs-0.2.1.9007019-ghc8_0_1-packages.conf.d/"]
    imgs = [("recta","recta.png"),("curva","curva.png"),("lava","lava.jpg"),("carro","carro.png")]

exCodeWorldGame :: CodeWorldArgs
exCodeWorldGame = CodeWorldArgs (Left "MMGame.hs") "Game" (CWGame CWGameConsole) ghcjs def "codeworld" [] []
    where
    ghcjs = def { ghcjsSafe = False }
--    db = ["../.cabal-sandbox/x86_64-osx-ghcjs-0.2.1.9007019-ghc8_0_1-packages.conf.d/"]



