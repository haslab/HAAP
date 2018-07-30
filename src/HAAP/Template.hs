{-
HAAP: Haskell Automated Assessment Platform

This module provides basic tmeplating functionality.
-}

{-# LANGUAGE DeriveGeneric, ScopedTypeVariables, ViewPatterns #-}

module HAAP.Template where
        
import HAAP.Core
import HAAP.IO
import HAAP.Shelly

import qualified Data.Text.Template as T
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import Data.String
import Data.Typeable
import Data.Binary
import qualified Data.Map as Map
import Data.Map (Map(..))
import Data.Default

import GHC.Generics (Generic, Generic1)

import Shelly (Sh(..))
import qualified Shelly as Sh

import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Configuration
import           Hakyll.Core.Item
import           Hakyll.Core.Provider
import qualified Hakyll.Core.Store             as Store
import           Hakyll.Core.Util.File
import           Hakyll.Core.Writable

type HaapTemplate = T.Template
newtype HaapContext = HaapContext { unHaapContext :: Map String String }
  deriving (Generic,Eq,Ord,Show,Typeable)

instance Binary HaapContext

unHaapConText :: HaapContext -> (Text.Text -> Text.Text)
unHaapConText (HaapContext f) = Text.pack . mapFun f . Text.unpack

mapFun :: Ord a => Map a b -> (a -> b)
mapFun m x = case Map.lookup x m of
    Nothing -> error "map element not found"
    Just y -> y

mapComp :: (Ord a,Eq b) => Map a b -> Map b c -> Map a c
mapComp (Map.toList -> xs) (Map.toList -> ys) = Map.fromList [ (a,c) | (a,b) <- xs, (b',c) <- ys, b == b' ]

instance Monoid HaapContext where
    mempty = HaapContext Map.empty
    mappend (HaapContext f) (HaapContext g) = HaapContext (mapComp f g)

makeTemplate :: String -> HaapTemplate
makeTemplate str = T.template $ fromString str

applyTemplate :: HaapTemplate -> HaapContext -> String
applyTemplate t c = TextL.unpack $ T.render t (unHaapConText c)

shLoadApplyAndCopyTemplate :: HaapContext -> FilePath -> FilePath -> Sh ()
shLoadApplyAndCopyTemplate ctx from to = do
--    Sh.liftIO $ putStrLn $ "shFrom " ++ show from
--    Sh.liftIO $ putStrLn $ "shTo " ++ show to
    txt::Text.Text <- Sh.readfile (shFromFilePath from)
    let tplt::T.Template = T.template txt
    let ctx' :: T.Context = unHaapConText ctx
    let txt'::TextL.Text = T.render tplt ctx'
    Sh.writefile (shFromFilePath to) (TextL.toStrict txt')

fieldContext :: String -> String -> HaapContext
fieldContext k v = HaapContext $ Map.singleton k v


data CopyHaapTemplateFile = CopyHaapTemplateFile HaapContext FilePath
    deriving (Generic, Eq,Ord,Show,Typeable)

instance Binary CopyHaapTemplateFile

--------------------------------------------------------------------------------
instance Writable CopyHaapTemplateFile where
    write dst (Item _ (CopyHaapTemplateFile ctx src)) = do
        runShCoreIO def $ shLoadApplyAndCopyTemplate ctx src dst
        
--------------------------------------------------------------------------------
copyHaapTemplateFileCompiler :: HaapContext -> Compiler (Item CopyHaapTemplateFile)
copyHaapTemplateFileCompiler ctx = do
    identifier <- getUnderlying
    provider   <- compilerProvider <$> compilerAsk
    makeItem $ CopyHaapTemplateFile ctx $ resourceFilePath provider identifier
