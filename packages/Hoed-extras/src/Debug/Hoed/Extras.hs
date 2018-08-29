{-# LANGUAGE PackageImports #-}

module Debug.Hoed.Extras (
    runHoedExtrasO, defaultHoedExtrasArgs
    , hoedExtrasDataPath, HoedExtrasArgs(..), HoedExtra(..)
    ) where

import Paths_Hoed_extras

import "Hoed" Debug.Hoed
import Debug.Hoed.Utils
import qualified Debug.Hoed.GHood as GHood
import qualified Debug.Hoed.GHoed as GHoed
import qualified Debug.Hoed.JsHoed as JsHoed
import qualified Debug.Hoed.JsHood as JsHood
import qualified "debug" Debug.DebugTrace as Debug
import qualified "debug" Debug.Hoed as Debug

runHoedExtrasO :: HoedExtrasArgs -> IO a -> IO ()
runHoedExtrasO extras prog = do
    h <- runO' (options extras) prog
    GHood.ghoodExtra extras h
    JsHood.jshoodExtra extras h
    GHoed.ghoedExtra extras h
    JsHoed.jshoedExtra extras h
    debugExtra extras h 

defaultHoedExtrasArgs :: HoedExtrasArgs
defaultHoedExtrasArgs = HoedExtrasArgs defaultHoedOptions Nothing None None None None None

debugExtra :: HoedExtrasArgs -> HoedAnalysis -> IO ()
debugExtra args h = case debug args of
    None -> return ()
    View -> do
        let opts' = (options args) {Debug.prettyWidth = 160, Debug.verbose = Verbose}
        let !compTree = hoedCompTree h
        let trace = Debug.convert compTree
        Debug.debugViewTrace trace
    Deploy -> do
        let opts' = (options args) {Debug.prettyWidth = 160, Debug.verbose = Verbose}
        let !compTree = hoedCompTree h
        let trace = Debug.convert compTree
        Debug.debugSaveTrace "debug.html" trace
     



