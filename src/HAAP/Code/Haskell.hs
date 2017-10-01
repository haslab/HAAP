module HAAP.Code.Haskell where

import HAAP.Core
import HAAP.IO

import Language.Haskell.Exts

import Control.Monad.Except

parseHaskellFile :: HaapMonad m => FilePath -> Haap p args db m (Module SrcSpanInfo)
parseHaskellFile file = do
    str <- runIO $ readFile file
    let mb = readExtensions str
    let mblang = join $ fmap fst mb
    let exts = maybe [] snd mb
    let mode' = defaultParseMode { parseFilename = file, extensions = exts }
    let mode'' = case mblang of { Nothing -> mode'; Just lang -> mode' {baseLanguage = lang } }
    let res = parseWithMode mode'' str
    case res of
        ParseOk m -> return m
        err -> throwError $ HaapException $ show err

parseModuleFileName :: HaapMonad m => FilePath -> Haap p args db m String
parseModuleFileName file = do
    m <- parseHaskellFile file
    return $ moduleName m

moduleName :: Module loc -> String
moduleName (Module _ n _ _ _) = maybe "Main" moduleHeadName n
moduleName (XmlPage l n _ _ _ _ _) = moduleNameString n
moduleName (XmlHybrid l n _ _ _ _ _ _ _) = maybe "Main" moduleHeadName n
    
moduleHeadName :: ModuleHead loc -> String
moduleHeadName (ModuleHead l n _ _) = moduleNameString n

moduleNameString :: ModuleName loc -> String
moduleNameString (ModuleName _ n) = n