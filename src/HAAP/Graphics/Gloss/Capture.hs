{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Gloss@ plugin for the animation of _gloss_ (<https://hackage.haskell.org/package/gloss>) code as a video.

-}

{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, EmptyDataDecls, CPP, TupleSections #-}

module HAAP.Graphics.Gloss.Capture where

import HAAP.IO
import HAAP.Shelly
import HAAP.Core
import HAAP.Log
import HAAP.Plugin

import Codec.Picture.Types (Image(..), PixelRGBA8)
import Codec.Picture.Png (writePng)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import qualified Data.Vector.Storable as Vector
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Rendering as Gloss
--import Graphics.Rendering.OpenGL.Raw -- as gl*
import Graphics.GL.Compatibility45
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GL
import Foreign (ForeignPtr, newForeignPtr_)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Text.Printf (printf)
import Data.StateVar
import Data.Proxy

import Control.Monad.Trans.Compose
import Control.Monad.Reader as Reader
import Control.Monad.IO.Class
import Control.Monad 
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Morph

data Gloss

data GlossArgs = GlossArgs
    { glossWindowSize :: (Int,Int) -- in pixels
    , glossFrameRate :: Int -- game's frame rate
    , glossAnimationTime :: Int -- total animation time
    , glossXvfb :: Bool -- run OpenGL inside xvfb for headless X servers
    }

instance HaapPlugin Gloss where
    type PluginI Gloss = GlossArgs
    type PluginT Gloss = ReaderT GlossArgs
    type PluginO Gloss = ()
    type PluginK Gloss t m = (MonadIO m)
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . getComposeT) m
        return (x,())

instance (HaapMonad m) => HasPlugin Gloss (ReaderT GlossArgs) m where
    liftPlugin = id
--instance (HaapStack t2 m,HaapPluginT (ReaderT GlossArgs) m (t2 m)) => HasPlugin Gloss (ComposeT (ReaderT GlossArgs) t2) m where
--    liftPlugin m = ComposeT $ hoistPluginT liftStack m

-- for GLFW hackage package
initOpenGL :: Int -> Int -> IO ()
initOpenGL windowWidth windowHeight = do
    ok1 <- GLFW.initialize
--    logEvent $ "initialization " ++ show ok1
    ok2 <- GLFW.openWindow
                (GL.Size (toEnum windowWidth) (toEnum windowHeight)) [] GLFW.Window
--    logEvent $ "opening windows " ++ show ok2
    GLFW.windowTitle $=! "GlossCapture"
    return ()

-- for GLFW-b hackage package
--initOpenGL :: Int -> Int -> IO ()
--initOpenGL windowWidth windowHeight = do
--    True <- ioRetry 5 GLFW.init not (finalize >> ioSleep 2)
--    GLFW.windowHint $ GLFW.WindowHint'Visible False
--    Just w <- GLFW.createWindow
--                windowWidth windowHeight
--                "gloss-to-file demo"
--                Nothing Nothing
--    GLFW.makeContextCurrent (Just w)

drawFrame :: Int -> Int -> Gloss.State -> Gloss.Picture -> IO ()
drawFrame windowWidth windowHeight s p = Gloss.withClearBuffer Gloss.black
            $ Gloss.withModelview (windowWidth, windowHeight)
            $ do
    glColor3f 1 1 1
    Gloss.renderPicture s 1 p

initialize :: Int -> Int -> IO Gloss.State
initialize w h = do
    s <- Gloss.initState
    initOpenGL w h
    return s

-- for GLFW hackage package
finalize :: IO ()
finalize = GLFW.closeWindow

-- for GLFW-b hackage package
--finalize :: IO ()
--finalize = GLFW.terminate

saveFrameImpl :: Int -> Int -> Gloss.State -> FilePath -> Gloss.Picture -> IO ()
saveFrameImpl windowWidth windowHeight s f p = do
    glDrawBuffer GL_BACK
    drawFrame windowWidth windowHeight s p
    glReadBuffer GL_BACK
    imageData <- mallocArray (windowWidth * windowHeight * 4)
    glReadPixels 0 0 (toEnum windowWidth) (toEnum windowHeight) GL_RGBA GL_UNSIGNED_BYTE imageData
    
    -- save the result
    foreignPtr <- newForeignPtr_ imageData
    let vector = unsafeFromForeignPtr0 foreignPtr (windowWidth * windowHeight * 4)
    let image :: Image PixelRGBA8
        image = Image windowWidth windowHeight vector
    writePng f (Image windowWidth windowHeight vector :: Image PixelRGBA8)
    
    free imageData


saveFrame :: Int -> Int -> FilePath -> Gloss.Picture -> IO ()
saveFrame windowWidth windowHeight f p = do
    s <- initialize windowWidth windowHeight
    saveFrameImpl windowWidth windowHeight s f p
    finalize


type Animation a = a -> Float -> IO (Maybe a,Gloss.Picture)

-- FilePath must contain "%d", will be replaced by frame number
saveFrames :: Int -> Int -> FilePath -> Animation a -> [Float] -> a -> IO a
saveFrames windowWidth windowHeight f anim ts x = do
    s <- initialize windowWidth windowHeight
    x' <- advance s x (zip [1..] ts)
    finalize
    return x'
  where
    advance s world [] = return world
    advance s world ((n,t):ts) = do
        let filename = printf f (n :: Int)
        (mb,pict) <- anim world t
        saveFrameImpl windowWidth windowHeight s filename pict
        case mb of
            Nothing -> return world
            Just world' -> advance s world' ts

-- records a video and returns the final state
record :: (MonadIO m,HasPlugin Gloss t m) => Animation a -> a -> FilePath -> Haap t m a
record anima gloss file = do
    args <- liftHaap $ liftPluginProxy (Proxy::Proxy Gloss) $ Reader.ask
    let (x,y) = glossWindowSize args
    let width = x
    let height = y
    let rate = glossFrameRate args
    let times = map fromIntegral [0..glossAnimationTime args*rate]
    gloss' <- runBaseIO $ saveFrames width height (file++"%04d.png") anima times gloss
    let ffargs = ["-framerate",show rate++"/1","-i",file++"%04d.png","-vf","hflip","-c:v","libx264","-r",show (max rate 30),"-pix_fmt","yuv420p",file++".mp4"]
    
    let addXvfb xs = if glossXvfb args then ["xvfb-run","-a","--server-args","\'-screen 0 1280x1024x24\'"]++xs else xs
    
    let ioargs = defaultIOArgs { ioSilent = True }
    logEvent "Generating video"
    runBaseSh $ do
        let (cmd:cmds) = addXvfb ("ffmpeg":ffargs)
        shCommandWith_ ioargs cmd cmds
    logEvent "Cleaning pngs"
    runBaseSh $ do
        let (cmd:cmds) = addXvfb ["rm","-rf",file++"*.png"]
        shCommandWith_ ioargs cmd cmds
    return gloss'