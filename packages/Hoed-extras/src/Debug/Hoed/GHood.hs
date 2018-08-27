{-# LANGUAGE RecordWildCards, PackageImports #-}

module Debug.Hoed.GHood where

import qualified Data.Vector.Generic as GV

import "Hoed" Debug.Hoed
import Debug.Hoed.Utils
import Debug.Hoed.Observe

import           System.IO
import           System.IO.Unsafe
import System.Process
import Data.IORef
import qualified Data.Text as T

import Control.Monad

import Paths_Hoed_extras

ghoodExtra :: HoedExtrasArgs -> HoedAnalysis -> IO ()
ghoodExtra args h = case ghood args of
    None -> return ()
    View -> debugSession True (hoedTrace h)
    Deploy -> debugSession False (hoedTrace h)

runGHoodO :: IO a -> IO ()
runGHoodO = runGHoodOwith defaultHoedOptions

runGHoodOwith :: HoedOptions -> IO a -> IO ()
runGHoodOwith options program = do
  HoedAnalysis{..} <- runO' options program
  debugSession True hoedTrace
  return ()

-- * GHood connection

debugSession :: Bool -> Trace -> IO ()
debugSession doCall es = do
    connectBrowser
    iMapM_ (\i v -> sendBrowser $ toBrowser i v) es
    disconnectBrowser doCall

iMapM_ :: (Monad m,GV.Vector v b) => (Int -> b -> m ()) -> v b -> m ()
iMapM_ f = GV.ifoldl g (return ()) . GV.unsafeTail
    where
    g m i v = m >> f (succ i) v

-- Connection to GHood graphical browser (via eventsHook).

observeEventsLog = "ObserveEvents.log"

call_GHood :: IO ()
call_GHood       = do
   ghood <- getDataFileName "GHood.jar"
   let call = "java -cp \"" ++ ghood ++ "\" GHood " ++ observeEventsLog
   hPutStrLn stderr call   
   system call
   return ()

toBrowser :: UID -> Event -> String
toBrowser eid e = 
       show eid
       ++ " " ++ show (max 0 $ parentUID (eventParent e))
       ++ " " ++ show (parentPosition (eventParent e))
       ++ " " ++ (case change e of
              { Observe s -> "Observe |" ++ T.unpack (trimText s)
              ; Cons n  s -> "Cons " ++ show n ++ " |" ++ T.unpack s
              ; ConsChar c -> [c]
              ; Enter     -> "Enter"
              ; Fun       -> "Fun"
              })

{-# NOINLINE global_Browser_pipe_ref #-}
global_Browser_pipe_ref :: IORef Handle
global_Browser_pipe_ref = unsafePerformIO $ 
  newIORef (error "not connected to GHood browser")

connectBrowser :: IO ()
connectBrowser = 
  do
    pipe <- openFile observeEventsLog WriteMode
    writeIORef global_Browser_pipe_ref pipe

disconnectBrowser :: Bool -> IO ()
disconnectBrowser call = 
  do
    pipe <- readIORef global_Browser_pipe_ref
    writeIORef global_Browser_pipe_ref (error "not connected to Browser")
    hClose pipe
    when call $ call_GHood

sendBrowser :: String -> IO ()
sendBrowser cmd = 
  do
    pipe <- readIORef global_Browser_pipe_ref
    hPutStrLn pipe cmd
    hFlush pipe


