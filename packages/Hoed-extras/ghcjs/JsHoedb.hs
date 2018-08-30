{-# LANGUAGE PackageImports, OverloadedStrings, ScopedTypeVariables #-}

module Main where
    
import Debug.Hoed
import Debug.Hoed.Observe
import Debug.Hoed.CompTree
import Debug.Hoed.CompTree.Exts

import Debug.Trace

import Safe

import Data.Graph.Libgraph hiding (Right)
import qualified Data.Graph.Libgraph as G

import Data.List as List
import Data.IORef
import Control.Monad

import Text.Read

import GHCJS.Types (JSVal(..), JSString)
import GHCJS.Foreign (jsNull, jsUndefined)
import GHCJS.Foreign.Callback (syncCallback, asyncCallback, syncCallback1, asyncCallback1, syncCallback2, asyncCallback2, OnBlocked(..))
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import Data.JSString.Text
import GHCJS.DOM.Types
import Control.Applicative ((<$>))
import GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import GHCJS.DOM.Enums
import           GHCJS.Foreign.Callback
import GHCJS.DOM.HTMLTextAreaElement
import GHCJS.DOM.KeyboardEvent
import           GHCJS.DOM
import           GHCJS.DOM.HTMLTextAreaElement
import           GHCJS.DOM.HTMLMediaElement as HTML
import           GHCJS.DOM.HTMLImageElement
import           GHCJS.DOM.NonElementParentNode
import           GHCJS.DOM.GlobalEventHandlers as Global hiding (error)
import           GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Node as Node
import           "ghcjs-dom" GHCJS.DOM.Document
import qualified GHCJS.DOM.DOMRect as DOMRect
import qualified GHCJS.DOM.DOMRectReadOnly as DOMRectRO
import           GHCJS.DOM.Element
import           GHCJS.DOM.EventM
import           GHCJS.DOM.MouseEvent
import           GHCJS.DOM.Types (Element, unElement, HTMLImageElement)
import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Types
import GHCJS.Fetch

main = do
    st <- initializeJsHoed
    drawJsHoed st

-- * Static

getCompTree :: IO CompTree
getCompTree = do
    fetchBinaryFile "CompTree"

drawJsHoed :: JsHoed -> IO ()
drawJsHoed st = do
    compTree <- readIORef $ jsCompTree st
    drawCompTree compTree
    currentVertex <- readIORef $ jsCurrentVertex st
    forM_ currentVertex $ selectNode . vertexId

drawCompTree :: CompTree -> IO ()
drawCompTree tree = drawCompTreeFrom tree (G.root tree) faults
    where
    faults = faultyVertices tree

drawCompTreeFrom :: CompTree -> Vertex -> [Vertex] -> IO ()
drawCompTreeFrom tree v faults = do
    drawVertex v (elem v faults)
    let nexts = succArcs tree v
    mapM_ (drawSuccArc tree faults) nexts
    
drawSuccArc :: CompTree -> [Vertex] -> Arc Vertex () -> IO ()
drawSuccArc tree faults (Arc src tgt _) = do
    drawCompTreeFrom tree tgt faults
    addEdge (vertexId src) (vertexId tgt)

drawVertex :: Vertex -> Bool -> IO ()
drawVertex RootVertex isFault = addRootNode
drawVertex (Vertex s j) isFault = do
    addNode (stmtIdentifier s) (toJSString $ prettyCompStmt s) (toJSString $ showJudgement j isFault)

vertexId :: Vertex -> Int
vertexId RootVertex = 0
vertexId (Vertex s j) = stmtIdentifier s

showJudgement :: Judgement -> Bool -> String
showJudgement _ True = "faulty"
showJudgement G.Right False = "right"
showJudgement G.Wrong False = "wrong"
showJudgement G.Unassessed	False = "unassessed"
showJudgement _ _ = error "unknown judgement"

foreign import javascript unsafe "addRootNode()"
        addRootNode :: IO ()

foreign import javascript unsafe "addNode({id: $1, label: $2, judgement: $3})"
        addNode :: Int -> JSString -> JSString -> IO ()
        
foreign import javascript unsafe "selectNode($1)"
        selectNode :: Int -> IO ()

foreign import javascript unsafe "deselectNode($1)"
        deselectNode :: Int -> IO ()

foreign import javascript unsafe "addEdge({src: $1, tgt: $2})"
        addEdge :: Int -> Int -> IO ()
        
--foreign import javascript unsafe "highlightNode({id: $1})"
--        highlightNode :: Int -> IO ()

foreign import javascript unsafe "updateStatus($1,$2)"
        updateStatus :: JSString -> Bool -> IO ()
        
foreign import javascript unsafe "judgeNode({id: $1, judgement: $2})"
        judgeNode :: Int -> JSString -> IO ()

updateJsHoedStatus :: JsHoed -> IO ()
updateJsHoedStatus st = do
    g <- readIORef $ jsCompTree st
    let isJudged v = case getJudgement v of
          G.Right -> True
          G.Wrong -> True
          _     -> False -- Assisted does not count as judged
        slen       = show . length
        ns = filter (not . isRootVertex) (preorder g)
        (js,remainingjs) = partition isJudged ns
        fs = faultyVertices g
        isFault = (length fs > 0) || null remainingjs
        txt = if (length fs > 0) then " Fault detected in: " ++ (prettyVertex . head) fs
                                 else " Judged " ++ slen js ++ "/" ++ slen ns
    updateStatus (toJSString txt) isFault
    forM_ fs $ \f -> judgeNode (vertexId f) (toJSString $ showJudgement G.Right True)

foreign import javascript unsafe "document.getElementById($1).addEventListener('click',$2);"
    onClick :: JSString -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "document.getElementById($1).addEventListener('change',function(){ return $2(this.value); });"
    onRadioChange :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "network.on('selectNode',function(params) { params.nodes.forEach($1);} );"
    onSelectNode :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "network.on('deselectNode',function(params) { params.previousSelection.nodes.forEach($1);} );"
    onDeselectNode :: Callback (JSVal -> IO ()) -> IO ()

-- * Judge

judgeListeners :: JsHoed -> IO ()
judgeListeners st = do
    callbackRight <- syncCallback ContinueAsync $ do
        judgeJsHoed G.Right st
    onClick "right" callbackRight
    
    callbackWrong <- syncCallback ContinueAsync $ do
        judgeJsHoed G.Wrong st
    onClick "wrong" callbackWrong
        
currentVertexListeners :: JsHoed -> IO ()
currentVertexListeners st = do
    callBackSelect <- syncCallback1 ContinueAsync $ \val -> do
        Just nodeId <- fromJSVal val
        Just v <- lookupVertexJsHoed nodeId st
        writeIORef (jsCurrentVertex st) (Just v)
    onSelectNode callBackSelect
        
    callBackDeselect <- syncCallback1 ContinueAsync $ \nodeId -> do
        writeIORef (jsCurrentVertex st) Nothing
    onDeselectNode callBackDeselect

nextListeners :: JsHoed -> IO ()
nextListeners st = do
    
    callbackNext <- syncCallback ContinueAsync $ do
        advanceJsHoed st
    onClick "next" callbackNext
    
    callbackNextStrat <- syncCallback1 ContinueAsync $ \val -> do
        Just (str::String) <- fromJSVal val
        case str of
            "step" -> writeIORef (jsNext st) next_step
            "daq" -> writeIORef (jsNext st) next_daq
            otherwise -> return ()
    onRadioChange "step" callbackNextStrat
    onRadioChange "daq" callbackNextStrat

judgeJsHoed :: Judgement -> JsHoed -> IO ()
judgeJsHoed j st = do
    ct <- readIORef (jsCompTree st)
    mbv <- readIORef (jsCurrentVertex st)
    forM_ mbv $ \v -> do
        writeIORef (jsCompTree st) =<< markNode ct v j
        updateJsHoedStatus st
    advanceJsHoed st
        
advanceJsHoed :: JsHoed -> IO ()
advanceJsHoed st = do
    t <- readIORef (jsCompTree st)
    mv <- readIORef (jsCurrentVertex st)
    next <- readIORef (jsNext st)
    case (next t getJudgement) of
        RootVertex -> return ()
        w -> advanceTo w st

advanceTo :: Vertex -> JsHoed -> IO ()
advanceTo v st = do
    currentVertex <- readIORef (jsCurrentVertex st)
    forM_ currentVertex $ deselectNode . vertexId
    writeIORef (jsCurrentVertex st) (Just v)
    selectNode (vertexId v)

markNode :: CompTree -> Vertex -> Judgement -> IO CompTree
markNode g v s = mapGraphM f g
  where f RootVertex = return RootVertex
        f v'         = if v' === v then judgeNode (vertexId v) (toJSString $ showJudgement s False) >> return (setJudgement v s) else return v'

        (===) :: Vertex -> Vertex -> Bool
        v1 === v2 = (vertexId v1) == (vertexId v2)

-- * State

data JsHoed = JsHoed
    { jsCompTree :: IORef CompTree
    , jsCurrentVertex :: IORef (Maybe Vertex)
    , jsNext :: IORef Next
    }
    
type Next = CompTree -> (Vertex -> Judgement) -> Vertex

initializeJsHoed :: IO JsHoed
initializeJsHoed = do
--    putStrLn "initialaizing"
    compTree <- getCompTree
--    putStrLn "loaded comptree"
    -- Get a list of vertices from the computation graph
    let ns = filter (not . isRootVertex) (preorder compTree)
    let isFault = length ns == 0
    updateStatus (toJSString $ " Judged 0/" ++ show (length ns)) isFault
    let currentVertex = headMay $ ns
    compTreeRef <- newIORef compTree
    currentVertexRef <- newIORef currentVertex
    nextRef <- newIORef next_step
    let st = JsHoed compTreeRef currentVertexRef nextRef
    judgeListeners st
    currentVertexListeners st
    nextListeners st
    return st

lookupVertexJsHoed :: Int -> JsHoed -> IO (Maybe Vertex)
lookupVertexJsHoed nodeId st = do
    t <- readIORef (jsCompTree st)
    return $ List.find (\v -> vertexId v == nodeId) (vertices t)

-- * Utils

succArcs :: Eq vertex => Graph vertex arc -> vertex -> [Arc vertex arc]
succArcs g v = filter ((== v) . source) (arcs g)

preorder :: CompTree -> [Vertex]
preorder = getPreorder . getDfs

orError str m = m >>= \x -> case x of
    Nothing -> Prelude.error $ str
    Just x -> return x

faultyVertices :: CompTree -> [Vertex]
faultyVertices = findFaulty_dag getJudgement

mapGraphM :: Monad m => (a -> m b) -> Graph a c -> m (Graph b c)
mapGraphM f (Graph r vs as) = do
    r' <- (f r)
    vs' <- (mapM f vs)
    as' <- (mapArcsVM f as)
    return $ Graph r' vs' as'

mapArcsVM :: Monad m => (a -> m b) -> [Arc a c] -> m [Arc b c]
mapArcsVM f = mapM (mapArcVM f)

mapArcVM :: Monad m => (a -> m b) -> Arc a c -> m (Arc b c)
mapArcVM f (Arc src tgt t) = do
    src' <- (f src)
    tgt' <- (f tgt)
    return $ Arc src' tgt' t


