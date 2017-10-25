module HAAP.Code.Haskell where

import HAAP.Core
import HAAP.IO
import HAAP.Pretty

import Data.Generics
import Data.List as List
import Data.Maybe

import Language.Haskell.Exts

import Control.Monad.Except

instance Show a => Out (ParseResult a) where
    docPrec i x = doc x
    doc (ParseOk x) = text "parsing ok:" <+> text (show x)
    doc (ParseFailed l s) = text "parsing failed at" <+> text (show l) <> char ':' <+> text s

parseHaskellFile :: HaapMonad m => FilePath -> Haap p args db m (Module SrcSpanInfo)
parseHaskellFile file = do
    str <- runIO' $ readFile file
    let mb = readExtensions str
    let mblang = join $ fmap fst mb
    let exts = maybe [] snd mb
    let mode' = defaultParseMode { parseFilename = file, extensions = exts }
    let mode'' = case mblang of { Nothing -> mode'; Just lang -> mode' {baseLanguage = lang } }
    let res = parseWithMode mode'' str
    case res of
        ParseOk m -> return m
        err -> throwError $ HaapException $ pretty err

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

getTopDecls :: Data a => a -> [Decl SrcSpanInfo]
getTopDecls x = everythingBut (++) (mkQ ([],False) aux) x
    where
    aux :: Decl SrcSpanInfo -> ([Decl SrcSpanInfo],Bool)
    aux d = ([d],True)

noLocDecl :: Decl l -> Decl ()
noLocDecl = noloc

noloc :: Functor a => a l -> a ()
noloc = fmap (const ())

removeTopConstants :: [Decl SrcSpanInfo] -> [Decl SrcSpanInfo]
removeTopConstants xs = catMaybes $ List.map removeTopConstant xs

removeTopConstant :: Decl SrcSpanInfo -> Maybe (Decl SrcSpanInfo)
removeTopConstant (PatBind _ (PVar {}) _ _) = Nothing
removeTopConstant (FunBind _ [Match _ _ [] _ _]) = Nothing
removeTopConstant d = Just d

functions :: Data a => a -> [Decl SrcSpanInfo]
functions = everything (++) (mkQ [] decls)
    where
    decls :: Decl SrcSpanInfo -> [Decl SrcSpanInfo]
    decls (PatBind l p r _) = [PatBind l p r $ Just $ BDecls l []]
    decls (FunBind l xs) = List.map match xs
    decls _ = []
    match :: Match SrcSpanInfo -> Decl SrcSpanInfo
    match  (Match l n ps r _) = FunBind l [Match l n ps r $ Just $ BDecls l []]

removeLets :: Data a => a -> a
removeLets = everywhere (mkT unlet)
    where
    unlet :: Exp SrcSpanInfo -> Exp SrcSpanInfo
    unlet (Let l _ e) = Let l (BDecls l []) e
    unlet e = e

