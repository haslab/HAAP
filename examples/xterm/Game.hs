import Data.String
import qualified Data.Text as T

import Anima

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

main :: IO ()
main = do
    plays <- playmatch [greedyBot,outraBot]
    mainAnimate plays

greedyBot = mkBot greedy
outraBot = mkBot outra

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

orError str m = m >>= \x -> case x of
    Nothing -> Prelude.error $ str
    Just x -> return x

type Play = Either String String
type Plays = [Play]

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

