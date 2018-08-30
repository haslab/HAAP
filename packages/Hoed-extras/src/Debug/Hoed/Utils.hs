{-# LANGUAGE OverloadedStrings, PackageImports, StandaloneDeriving #-}

module Debug.Hoed.Utils where

import "Hoed" Debug.Hoed
import Debug.Hoed.Observe

import Paths_Hoed_extras

import Data.String
import Data.Text.Template as Template
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as UV
import Data.List

data HoedExtrasArgs = HoedExtrasArgs
    { options :: HoedOptions
    , datapath :: Maybe (IO FilePath)
    , ghood :: HoedExtra
    , jshood :: HoedExtra
    , ghoed :: HoedExtra
    , jshoed :: HoedExtra -- pass CompTree in String format (show/red)
    , jshoedb :: HoedExtra -- pass CompTree in ByteString format (encode/decode)
    , debug :: HoedExtra
    }

data HoedExtra = View | Deploy | None

hoedExtrasDataPath :: IO FilePath
hoedExtrasDataPath = getDataFileName "."

-- * Additional instances

deriving instance Ord Parent

-- * Template functions

makeTemplate :: String -> Template
makeTemplate str = template $ fromString str

readTemplateFile :: FilePath -> IO Template
readTemplateFile fn = makeTemplate <$> readFile fn

applyTemplate :: Template -> Context -> String
applyTemplate t c = TL.unpack $ render t c

-- * Utility functions to mitigate the debug library HACK (of adding code to observation labels)

trimText :: T.Text -> T.Text
trimText t = w
    where
    ws = T.lines t
    w = if null ws then "" else head ws

