{-# LANGUAGE PackageImports, OverloadedStrings, RecordWildCards, ViewPatterns, DeriveGeneric #-}

module Debug.Hoed.JsHood where

import "Hoed" Debug.Hoed
import Debug.Hoed.Utils
import Debug.Hoed.Observe
import Debug.Hoed.CompTree

import Data.Aeson as Aeson
import Data.Aeson.Encoding as Aeson

import Control.Monad
import Control.Monad.State (State(..))
import qualified Control.Monad.State as State
import Data.Maybe
import qualified Data.Vector.Generic as GV
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IMap
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.IntSet (IntSet(..))
import qualified Data.IntSet as ISet
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import GHC.Generics
import GHC.Word

import System.IO.Temp
import System.IO
import Web.Browser

import Paths_Hoed_extras

jshoodExtra :: HoedExtrasArgs -> HoedAnalysis -> IO ()
jshoodExtra args h = case jshood args of
    None -> return ()
    View -> debugSession (options args) h
    Deploy -> debugOutput (options args) h >>= writeFile "jshood.html" 

runJsHoodO :: IO a -> IO ()
runJsHoodO = runJsHoodOwith defaultHoedOptions

runJsHoodOwith :: HoedOptions -> IO a -> IO ()
runJsHoodOwith options program = do
    h <- runO' options program
    debugSession options h

debugOutput :: HoedOptions -> HoedAnalysis -> IO String
debugOutput options h = do
    let tr = hoedTrace h
    ti <- traceInfo (verbose options) tr
    let jstr = mkJsTrace tr ti
    let jsons = show $ map encode jstr
    let ctx "jsEvents" = T.pack jsons
        ctx n = n
    infn <- getDataFileName "web/jshood.html"
    tplt <- readTemplateFile infn
    let outstr = applyTemplate tplt ctx
    return outstr
 
debugSession :: HoedOptions -> HoedAnalysis -> IO ()
debugSession options h = do
    outstr <- debugOutput options h
    outfn <- writeSystemTempFile "jshood.html" outstr
    openBrowser outfn
    return ()

type JsId = Int

type JsLabel = String

data JsParent = JsParent { jsParentId :: JsId, jsParentType :: JsEdgeType }
  deriving (Generic,Show)

instance ToJSON JsParent where
    toEncoding = genericToEncoding defaultOptions

data JsEdgeType = JsInput | JsOutput | JsCall
  deriving (Generic,Show)

instance ToJSON JsEdgeType where
    toEncoding = genericToEncoding defaultOptions

data JsChild = JsChild { jsChildId :: JsId, jsChildType :: JsEdgeType }
  deriving (Generic,Show)

instance ToJSON JsChild where
    toEncoding = genericToEncoding defaultOptions

data JsEvent = JsEvent { jsId :: JsId, jsType :: JsType, jsChildren :: [JsChild], jsParent :: (Maybe JsParent), jsLabel :: (Maybe JsLabel) }
  deriving (Generic,Show)

data JsType = JsNew | JsUpd
  deriving (Generic,Show)
  
instance ToJSON JsType where
    toEncoding = genericToEncoding defaultOptions

jsNew :: JsId -> [JsChild] -> (Maybe JsParent) -> (Maybe JsLabel) -> JsEvent
jsNew jid jchi jpar jlbl = JsEvent jid JsNew jchi jpar jlbl

jsUpd :: JsId -> [JsChild] -> Maybe JsParent -> JsLabel -> JsEvent
jsUpd jid jchi jpar jlbl = JsEvent jid JsUpd jchi jpar (Just jlbl)
    
instance ToJSON JsEvent where
    toEncoding = genericToEncoding defaultOptions

type JsTrace = [JsEvent]
    
data JsState = JsState
    { jsLabels :: JsLabels
    , jsChilds :: JsChildren
    , jsAliases :: JsAliases
    , jsFuns :: JsFuns
    , jsObserveFuns :: JsFuns
    , jsDeadFuns :: JsFuns
    , jsTraceInfo :: JsTraceInfo
    }
  deriving (Generic,Show)

type JsLabels = IntMap JsLabel -- map from nodes to labels
type JsChildren = IntMap [Int] -- ordered list of children of a node
type JsAliases = Map Parent Parent
type JsFuns = IntSet

type JsTraceInfo = IntMap JsId -- map from childs to parents

initializeJsState :: Trace -> TraceInfo -> JsState
initializeJsState tr ti = st
    where
    emptyJsState = JsState IMap.empty IMap.empty Map.empty ISet.empty ISet.empty ISet.empty (mkJsTraceInfo ti)
    st = GV.ifoldl initializeJsEvent emptyJsState (GV.unsafeTail tr)
    initializeJsEvent :: JsState -> Int -> Event -> JsState
    initializeJsEvent st (succ -> uid) (Event parent change) = case change of
        Observe s -> addJsLabel uid (T.unpack $ trimText s) st
        Enter -> st
        Fun -> flip State.execState st $ do
            isParentFun <- State.gets $ isJsFun (parentUID parent)
            if isParentFun
                then do
                    State.modify $ addJsChildrenAliases uid parent
                    State.modify $ addJsDeadFun uid
                else State.modify $ addJsFun uid
            State.modify $ copyJsLabel (parentUID parent) uid
            State.modify $ addJsChild uid parent
        Cons n s -> addJsChild uid parent st
        ConsChar c -> addJsChild uid parent st

copyJsLabel :: JsId -> JsId -> JsState -> JsState
copyJsLabel from to st = case IMap.lookup from (jsLabels st) of
    Nothing -> st
    Just lbl -> st { jsLabels = IMap.insert to lbl $ jsLabels st, jsObserveFuns = ISet.insert from $ jsObserveFuns st }

isJsDeadFun :: JsId -> JsState -> Bool
isJsDeadFun uid st = ISet.member uid (jsDeadFuns st)

isJsObserveFun :: JsId -> JsState -> Bool
isJsObserveFun uid st = ISet.member uid (jsObserveFuns st)

addJsDeadFun :: JsId -> JsState -> JsState
addJsDeadFun uid st = st { jsDeadFuns = ISet.insert uid $ jsDeadFuns st }

addJsLabel :: JsId -> JsLabel -> JsState -> JsState
addJsLabel uid lbl st = st { jsLabels = IMap.insert uid lbl $ jsLabels st }

isJsFun :: JsId -> JsState -> Bool
isJsFun uid st = ISet.member uid (jsFuns st)

addJsFun :: JsId -> JsState -> JsState
addJsFun uid st = st { jsFuns = ISet.insert uid $ jsFuns st }

addJsChildrenAliases :: JsId -> Parent -> JsState -> JsState
addJsChildrenAliases uid p@(Parent puid pidx) st =
    st { jsAliases = Map.insert (Parent uid 1) (Parent puid $ pidx+1) $ Map.insert (Parent uid 0) p $ jsAliases st }

addJsChild :: JsId -> Parent -> JsState -> JsState
addJsChild uid p st = st { jsChilds = IMap.alter (Just . ins cpidx uid . maybe [] id) cpuid $ jsChilds st }
    where
    cp@(Parent cpuid cpidx) = canonicalParent p st
    ins :: (Num a) => Word8 -> a -> [a] -> [a]
    ins 0 v [] = [v]
    ins 0 v (x:xs) = v:xs
    ins n v [] = (-1) : ins (n-1) v []
    ins n v (x:xs) = x : ins (n-1) v xs

canonicalParent :: Parent -> JsState -> Parent
canonicalParent p st = case Map.lookup p (jsAliases st) of
    Nothing -> p
    Just p' -> canonicalParent p' st

mkJsTraceInfo :: TraceInfo -> JsTraceInfo
mkJsTraceInfo = IMap.foldrWithKey go IMap.empty . dependencies
    where
    go :: Int -> IntSet -> JsTraceInfo -> JsTraceInfo
    go from tos m | from > 0 = ISet.foldr (\to -> IMap.insert to from) m (ISet.filter (>0) tos)
                  | otherwise = m

mkJsTrace :: Trace -> TraceInfo -> JsTrace
mkJsTrace tr ti = reverse $ State.evalState (mkJsTraceSt tr) ini
    where ini = initializeJsState tr ti
    
mkJsTraceSt :: Trace -> State JsState JsTrace
mkJsTraceSt = GV.ifoldl addJsEventSt (return []) . GV.unsafeTail
    where
    addJsEventSt :: State JsState JsTrace -> Int -> Event -> State JsState JsTrace
    addJsEventSt mtr (succ -> i) e = do
        tr <- mtr
        te <- mkJsEventSt i e
        return (maybeToList te ++ tr)

mkJsEventSt :: Int -> Event -> State JsState (Maybe JsEvent)
mkJsEventSt uid (Event parent change) = case change of
    Observe s -> do
        isFun <- State.gets $ isJsObserveFun uid
        if isFun
            then return Nothing
            else do
                ch <- State.gets $ mkJsChildren uid
                par <- State.gets $ mkJsParent uid parent
                lbl <- State.gets $ mkJsLabel uid
                return $ Just $ jsNew uid ch par lbl
    Enter -> return Nothing
    Fun -> do
        isDeadFun <- State.gets $ isJsDeadFun uid
        if isDeadFun
            then return Nothing
            else do
                ch <- State.gets $ mkJsChildren uid
                par <- State.gets $ mkJsParent uid parent
                lbl <- State.gets $ mkJsLabel uid
                return $ Just $ jsNew uid ch par lbl
    Cons _ s -> do
        ch <- State.gets $ mkJsChildren uid
        par <- State.gets $ mkJsParent uid parent
        return $ Just $ jsUpd uid ch par (T.unpack s)
    ConsChar c -> do
        ch <- State.gets $ mkJsChildren uid
        par <- State.gets $ mkJsParent uid parent
        return $ Just $ jsUpd uid ch par [c]

mkJsChildren :: JsId -> JsState -> [JsChild]
mkJsChildren uid st = maybe [] add $ IMap.lookup uid (jsChilds st)
    where
    add xs = map (uncurry JsChild) $ zip xs $ replicate (length xs-1) JsInput ++ [JsOutput]

mkJsLabel :: JsId -> JsState -> Maybe JsLabel
mkJsLabel uid st = IMap.lookup uid (jsLabels st)

mkJsParent :: JsId -> Parent -> JsState -> Maybe JsParent
mkJsParent uid (Parent puid (fromEnum -> pidx)) st = case IMap.lookup uid (jsTraceInfo st) of
    Just puid -> Just $ JsParent puid JsCall
    Nothing -> if puid > 0
        then Just $ JsParent puid (if pidx == len-1 then JsOutput else JsInput)
        else Nothing
 where
    len = maybe 0 length $ IMap.lookup puid (jsChilds st)

