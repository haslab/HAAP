{-# LANGUAGE ScopedTypeVariables #-}

module HAAP.Template where
        
import HAAP.Core
import HAAP.IO

import qualified Data.Text.Template as T
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import Data.String

import Shelly (Sh(..))
import qualified Shelly as Sh

type HaapTemplate = T.Template
newtype HaapContext = HaapContext { unHaapContext :: String -> String }

unHaapConText :: HaapContext -> (Text.Text -> Text.Text)
unHaapConText (HaapContext f) = Text.pack . f . Text.unpack

instance Monoid HaapContext where
    mempty = HaapContext id
    mappend (HaapContext f) (HaapContext g) = HaapContext (g . f)

makeTemplate :: String -> HaapTemplate
makeTemplate str = T.template $ fromString str

applyTemplate :: HaapTemplate -> HaapContext -> String
applyTemplate t c = TextL.unpack $ T.render t (unHaapConText c)

shLoadApplyAndCopyTemplate :: HaapContext -> FilePath -> FilePath -> Sh ()
shLoadApplyAndCopyTemplate ctx from to = do
    txt::Text.Text <- Sh.readfile (shFromFilePath from)
    let tplt::T.Template = T.template txt
    let ctx' :: T.Context = unHaapConText ctx
    let txt'::TextL.Text = T.render tplt ctx'
    Sh.writefile (shFromFilePath to) (TextL.toStrict txt')

fieldContext :: String -> String -> HaapContext
fieldContext k v = HaapContext (\k' -> if k == k' then v else k')