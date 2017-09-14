{-# LANGUAGE EmptyDataDecls, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import HAAP

import Data.Default
import Data.Binary
import Data.Char
import Data.Monoid hiding ((<>))
import qualified Data.Map as Map

import System.Random.Shuffle
import System.Process

import GHC.Generics (Generic(..))

data Example

example :: Project Example
example = Project
    { projectName = "Example"
    , projectPath = "."
    , projectGroups = []
    , projectTasks = []
    }

data Example_Args = Example_Args
    { exDBArgs :: BinaryDBArgs Example_DB
    }

exampleArgs = Example_Args (BinaryDBArgs "db" emptyLi1DB)

emptyLi1DB = Example_DB (HaapTourneyDB 1 [])

exSpec :: HaapSpec
exSpec = bounded "x" [97,98,3,4] $ \x ->
          bounded "y" "abcd" $ \y -> 
          testEqual $ return (x,ord y)
          
exRankSpec :: Int -> HaapSpec
exRankSpec i = bounded "x" [1,2,3,4,5] $ \x -> testEqual $ return (x,i)

type Ex = Haap Example Example_Args (BinaryDB Example_DB)

data Example_DB = Example_DB
    { exTourneyDB :: HaapTourneyDB ExPlayer
    }
  deriving (Generic)
instance Binary Example_DB

lnsTourney :: DBLens (BinaryDB Example_DB) (HaapTourneyDB ExPlayer)
lnsTourney = DBLens
    (BinaryDBQuery exTourneyDB)
    (\st -> BinaryDBUpdate $ \db -> ((),db { exTourneyDB = st }) )

haap :: Ex HaapTestTableRes
haap = do
    res <- runSpecWith getSpecArgs exSpec
    return res

getSpecArgs :: Example_Args -> HaapSpecArgs
getSpecArgs = const $ HaapSpecArgs HaapSpecQuickCheck Nothing
    
exRankScore :: HaapTestRes -> FloatScore
exRankScore Nothing = FloatScore 1
exRankScore (Just err) = FloatScore 0

exRank :: HaapSpecRank Example Example_Args db Int FloatScore
exRank = HaapSpecRank "ranks.html" "Ranks" "Grupo" "Ranking" [1..10::Int] (exRankSpec) (return . exRankScore)

main = do
--    logger <- new Message
    let cfg = defaultConfiguration
    runHaap example exampleArgs $ useDB exDBArgs $ do
        (web1,spec) <- renderHaapSpecsWith getSpecArgs "spec.html" "Spec" [("spec1",exSpec)]
        
        (web2,rank) <- renderHaapSpecRankWith getSpecArgs exRank
        
        (web3,tour) <- renderHaapTourney exTourney
        
        (web4,hadck) <- runHaddock exHaddock
        
        (web5,hlint) <- runHLint exHLint
        
        (web6,homplexity) <- runHomplexity exHomplexity
        
        (_,web7,hpcs) <- runHpc exHpc $ do
            runIO $ system "./HPCTest < ints"
            runIO $ system "./HPCTest < ints"
            
        (web8,cws) <- runCodeWorld exCodeWorld
        
        let pageRule = do
            match (fromGlob ("templates/example.html")) $ do
                route idRoute
                compile templateBodyCompiler
            create ["example.html"] $ do
                route idRoute
                compile $ do
                    let exCtx = constField "projectpath" "."
                              `mappend` constField "spec" spec
                              `mappend` constField "rank" rank
                              `mappend` constField "tour" tour
                              `mappend` constField "haddock" hadck
                              `mappend` constField "hlint" hlint
                              `mappend` constField "homplexity" homplexity
                              `mappend` listField "hpcs" (field "hpc" (return . itemBody)) (mapM makeItem hpcs)
                              `mappend` listField "cws" (field "cw" (return . itemBody)) (mapM makeItem cws)
                    makeItem "" >>= loadAndApplyTemplate "templates/example.html" exCtx
        
        runHakyllWith cfg $ web1 >> web2 >> web3 >> web4 >> web5 >> web6 >> web7 >> web8 >> pageRule

data ExPlayer = ExPlayer (String,Bool)
 deriving (Eq,Ord,Show,Generic)
instance Binary ExPlayer

instance Out ExPlayer where
    docPrec i x = doc x
    doc (ExPlayer x) = text (fst x)

instance TourneyPlayer ExPlayer where
    isDefaultPlayer (ExPlayer (_,b)) = b
    defaultPlayer = ExPlayer ("random",True)

exTourney :: HaapTourney Example Example_Args (BinaryDB Example_DB) ExPlayer [Link]
exTourney = HaapTourney 10 "Tourney" "Grupo" grupos "torneio" lnsTourney match return (const $ return ())
    where
    grupos = map (ExPlayer . mapFst show) $ zip [1..] (replicate 90 False ++ replicate 10 True)
    match tno rno mno players = do
        players' <- runIO $ shuffleM players
        return (zip players' [1..],["link"])



exHaddock = HaddockArgs True "Ex" [] "." ["Example.hs"] "doc"
exHLint = HLintArgs True [] "." ["Example.hs"] "hlint.html"
exHomplexity = HomplexityArgs True [] "." ["../src/"] "homplexity.html"

exHpc :: HpcArgs Example_Args
exHpc = HpcArgs ["HPCTest"] getGHC getIO False "hpc"
    where
    getIO = const def
    getGHC = const def

exCodeWorld :: CodeWorldArgs Example_Args
exCodeWorld = CodeWorldArgs [("MMGame.hs",CWGame),("MMDraw.hs",CWDraw "Insira um caminho...")] getGHCJS getIO "codeworld" db
    where
    getIO = const def
    getGHCJS = const $ def { ghcjsSafe = False }
    db = ["../.cabal-sandbox/x86_64-osx-ghcjs-0.2.1.9007019-ghc8_0_1-packages.conf.d/"]



