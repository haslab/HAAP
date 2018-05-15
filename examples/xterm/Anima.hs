module Anima where

import JavaScript.Xterm
import Data.String
import qualified Data.Text as T

import Data.Typeable (Typeable)
import GHCJS.Types (JSVal(..), JSString)
import GHCJS.Foreign (jsNull, jsUndefined)
import GHCJS.Foreign.Callback (syncCallback, asyncCallback, syncCallback1, asyncCallback1, syncCallback2, asyncCallback2, OnBlocked(..))
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import Control.Monad 
import Control.Monad.Trans
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int64)
import Data.Word (Word, Word64)
import Data.Maybe (fromJust)
import Data.Traversable (mapM)
import GHCJS.DOM.Types
import GHCJS.DOM
import GHCJS.DOM.EventM
import Control.Applicative ((<$>))
import GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import GHCJS.DOM.Enums
import           GHCJS.DOM.Element
import           GHCJS.DOM.Document
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.KeyboardEvent
import GHCJS.DOM.GlobalEventHandlers


--import Play (playrandom)
import Next (avanca)
import Data.Maybe (isNothing)
import Haskassonne
import Text.XML.Light as XML
import System.Environment (getArgs)
import System.Process (readProcess)
import Control.Monad (unless)


orError str m = m >>= \x -> case x of
    Nothing -> Prelude.error $ str
    Just x -> return x

mainAnimate :: Plays -> IO ()
mainAnimate plays = do
    term <- newTerminal 
    window <- orError "display window" currentWindow
    doc <- orError "" currentDocument
    el <- orError "" $ getElementById doc (fromString "terminal" :: JSString)
    open term el 
    animate term plays

type Play = Either String String
type Plays = [Play]

animate :: Terminal -> Plays -> IO ()
animate term [] = return ()
animate term [play] = animatePlay term play
animate term (play:plays) = do
    animatePlay term play
    getln term
    animate term plays

animatePlay term (Left err) = do
    write term err
    Prelude.error err
animatePlay term (Right b) = do
    forM_ (lines b) $ writeln term
