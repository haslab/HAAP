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

import Data.String
import qualified Data.Text as T

import Control.Monad 
import Control.Exception
import Control.Monad.Trans
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int64)
import Data.Word (Word, Word64)
import Data.Maybe (fromJust)
import Data.Traversable (mapM)
import Safe


import Play
import Next (avanca)
import Data.Maybe (isNothing)
import Haskassonne
import Text.XML.Light as XML
import System.Environment (getArgs)
import System.Process 
import Control.Monad (unless)

import System.Directory
import System.FilePath
import Control.DeepSeq

example :: Project
example = Project
    { projectName = "xterm"
    , projectPath = "."
    , projectTmpPath = "tmp"
    , projectGroups = []
    , projectTasks = []
    }

main = do
    let cfg = HakyllArgs defaultConfiguration False True def
    runHaap example $ useHakyll cfg $ do
        
        logEvent "Running Game"
        plays <- runBaseIO' $ playmatch [mkBot greedyBot,mkBot outraBot]
        
        logEvent "Compiling Game"
        runBaseIO' $ do
            writeFile ("Game.hs") $
                "module Main where\n"++
                "import Anima\n"++
                "main = mainAnimate "++show plays ++ "\n"
        
        useAndRunXterm exXterm
        
        hakyllRules $ do
                
            create ["index.md"] $ do
                route (setExtension "html")
                compile $ do
                    makeItem ("#Xterm example\n[run](xterm/Game.jsexe/run.html)"::String) >>= renderPandoc
        
        return ()

exXterm :: XtermArgs
exXterm = XtermArgs (Left "Game.hs") "Game" ghcjs def "xterm"
    where
    ghcjs = def { ghcjsSafe = False }

-- * run game

type Play = Either String String
type Plays = [Play]
type Bot = Board -> IO Tile

mkBot :: (XML.Element -> IO XML.Element) -> Bot
mkBot f = \b -> do
    let inp = fromBoard b
    out <- f inp
    let Just play = toTile out
    return play

runBot :: Bot -> Board -> IO (Either String Tile)
runBot bot board = catch run (\(e::SomeException) -> return $ Left $ show e)
    where
    run = do
        res <- bot board
        return $! Right $! force res

playmatch :: [Bot] -> IO Plays
playmatch bots = do
    aux (Board [] (replicate (length bots) 0) (Just E)) bots
  where aux b l | isNothing (next b) = do
                      return [Right $ show b]
                | otherwise = do
                    let prev = Right $ show b
                    mbplay <- runBot (headNote "playmatch" l) b
                    case mbplay of
                        Left err -> return [prev,Left err]
                        Right play -> if possible b play
                            then do
                                let b' = b {terrain = terrain b ++ [play], next = Nothing}
                                b'' <- avanca b'
                                liftM (prev:) $ aux b'' (roda l)
                            else return [prev,Left $ "wrong play"]



roda l = tailNote "roda" l ++ [headNote "roda" l]