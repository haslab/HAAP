{-
HAAP: Haskell Automated Assessment Platform

This module provides functions for processing Haskell source code files.

-}

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module HAAP.Code.Haskell where

import HAAP.Core
import HAAP.IO
import HAAP.Pretty as PP
import HAAP.Plugin
import HAAP.Utils

import Data.Generics
import Data.List as List
import Data.Char
import Data.Maybe
import qualified Data.Map as Map 
import Data.Map (Map(..))

import Language.Haskell.Exts as H hiding (Pretty)

import System.FilePath.Find as FilePath
import System.FilePath

import Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec.Number as Parsec

import Control.Monad.IO.Class
--import Control.Monad.Except
import Control.Exception.Safe
import Control.Monad

import GHC.Stack hiding (SrcLoc(..))

instance Pretty SrcLoc where
    pretty = unsafeViaShow

instance Show a => Pretty (ParseResult a) where
    pretty (ParseOk x) = string "parsing ok:" <+> string (show x)
    pretty (ParseFailed l s) = string "parsing failed at" <+> string (show l) PP.<> char ':' <+> string s

parseHaskellWith :: (HasCallStack,Parseable a,MonadIO m,HaapStack t m) => String -> Maybe [Extension] -> Maybe [H.Fixity] -> Haap t m a
parseHaskellWith str arg_exts arg_fix = do
    let mb = readExtensions str
    let mblang = join $ fmap fst mb
    let exts = maybe (maybe [] snd mb) id arg_exts
    let mode' = defaultParseMode { extensions = exts, fixities = arg_fix }
    let mode'' = case mblang of { Nothing -> mode'; Just lang -> mode' {baseLanguage = lang } }
    let res = parseWithMode mode'' str
    case res of
        ParseOk m -> return m
        (ParseFailed loc err) -> do
            let stack = callStack
            throw $ HaapException stack $ prettyText loc <> ": " <> prettyText err

parseHaskellFileWith :: (HasCallStack,MonadIO m,HaapStack t m) => FilePath -> Maybe [Extension] -> Maybe [H.Fixity] -> Haap t m (Module SrcSpanInfo)
parseHaskellFileWith file arg_exts arg_fix = do
    str <- runBaseIO' $ readFile file
    let mb = readExtensions str
    let mblang = join $ fmap fst mb
    let exts = maybe (maybe [] snd mb) id arg_exts
    let mode' = defaultParseMode { parseFilename = file, extensions = exts, fixities = arg_fix }
    let mode'' = case mblang of { Nothing -> mode'; Just lang -> mode' {baseLanguage = lang } }
    let res = parseWithMode mode'' str
    case res of
        ParseOk m -> return m
        err -> do
            let stack = callStack 
            throw $ HaapException stack $ prettyText err

parseHaskell :: (Parseable a,MonadIO m,HaapStack t m) => String -> Haap t m a
parseHaskell str = parseHaskellWith str Nothing Nothing

parseHaskellFile :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m (Module SrcSpanInfo)
parseHaskellFile file = parseHaskellFileWith file Nothing Nothing

parseModuleFileName :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m String
parseModuleFileName file = do
    m <- parseHaskellFile file
    return $ moduleName m

moduleImports :: Module loc -> [ImportDecl loc]
moduleImports (Module _ _ _ i _) = i
moduleImports (XmlPage {}) = []
moduleImports (XmlHybrid _ _ _ i _ _ _ _ _) = i

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
    match (Match l n ps r _) = FunBind l [Match l n ps r $ Just $ BDecls l []]
    match (InfixMatch l p n ps rhs bs) = match $ Match l n (p:ps) rhs bs

numFunctionVars :: Data a => a -> Int
numFunctionVars = everything (+) (mkQ 0 decls)
    where
    decls :: Decl SrcSpanInfo -> Int
    decls (PatBind l p r _) = pred $ numPVars p
    decls (FunBind l xs) = sum $ List.map match xs
    decls _ = 0
    match :: Match SrcSpanInfo -> Int
    match (Match l n ps r _) = sum $ List.map numPVars ps
    match (InfixMatch l p n ps rhs bs) = match $ Match l n (p:ps) rhs bs

numPVars :: Data a => a -> Int
numPVars = everything (+) (mkQ 0 pvar)
    where
    pvar :: Pat SrcSpanInfo -> Int
    pvar (PVar {}) = 1
    pvar _ = 0

removeLets :: Data a => a -> a
removeLets = everywhere (mkT unlet)
    where
    unlet :: Exp SrcSpanInfo -> Exp SrcSpanInfo
    unlet (Let l _ e) = Let l (BDecls l []) e
    unlet e = e

moduFunctionNames :: Module SrcSpanInfo -> Map (Name ()) Bool
moduFunctionNames m = everything Map.union (mkQ Map.empty aux) m
    where
    aux :: Decl SrcSpanInfo -> Map (Name ()) Bool
    aux (TypeSig _ ns t) = Map.fromList $ zip (List.map noloc ns) (repeat $ isHO t)
    aux _ = Map.empty
    isHO :: Type SrcSpanInfo -> Bool
    isHO (TyForall _ _ _ t) = isHO t
    isHO (TyFun _ t1 t2) = isHO t1 || isHO t2
    isHO t = everything (||) (mkQ False aux1) t
        where
        aux1 :: Type SrcSpanInfo -> Bool
        aux1 (TyFun {}) = True
        aux1 _ = False

moduNames :: Module SrcSpanInfo -> Map (Name ()) Int
moduNames m = everything (Map.unionWith (+)) (mkQ Map.empty aux) m
    where
    aux :: Name SrcSpanInfo -> Map (Name ()) Int
    aux n = Map.singleton (noloc n) 1

nameString :: Name a -> String
nameString (Ident _ s) = s
nameString (Symbol _ s) = s

getTopName :: Data a => a -> Maybe (Name SrcSpanInfo)
getTopName x = everythingBut appendMaybe (mkQ (Nothing,False) aux) x
    where
    aux :: Name SrcSpanInfo -> (Maybe (Name SrcSpanInfo),Bool)
    aux d = (Just d,True)

getNumVars :: Data a => a -> Int
getNumVars x = everything (+) (mkQ 0 auxExp `extQ` auxPat) x
    where
    auxExp :: Exp SrcSpanInfo -> Int
    auxExp (Var {}) = 1
    auxExp _ = 0
    auxPat :: Pat SrcSpanInfo -> Int
    auxPat (PVar {}) = 1
    auxPat _ = 0

allExtensions :: [Extension]
allExtensions =
  concat [ [EnableExtension x] | x <- [minBound..maxBound] ]

readFixities :: String -> [H.Fixity]
readFixities content = concatMap readFixity ls
    where
    ls = lines content

readFixity :: String -> [H.Fixity]
readFixity str = case Parsec.parse go "fixities" str of
    Left err -> []
    Right f -> f
  where
    go = do
        typ <- Parsec.string "infixl" <||> Parsec.string "infixr" <||> Parsec.string "infix"
        Parsec.spaces
        i <- Parsec.int
        Parsec.spaces
        tok <- Parsec.many1 $ Parsec.satisfy (not . isSpace)
        Parsec.spaces
        case typ of
            "infix" -> return $ infix_ i [tok]
            "infixl" -> return $ infixl_ i [tok]
            "infixr" -> return $ infixr_ i [tok]

-- * Find Haskell files

hsFiles :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m [FilePath]
hsFiles path = orDefault [] $ liftIO $ FilePath.fold (depth >=? 0) addHsFile [] path
    where
    addHsFile :: [FilePath] -> FileInfo -> [FilePath]
    addHsFile t i = if evalClause (extension ==? ".hs") i
        then let p = evalClause filePath i in t++[p]
        else t

lhsFiles :: (MonadIO m,HaapStack t m) => FilePath -> Haap t m [FilePath]
lhsFiles path = orDefault [] $ liftIO $ FilePath.fold (depth >=? 0) addHsFile [] path
    where
    addHsFile :: [FilePath] -> FileInfo -> [FilePath]
    addHsFile t i = if evalClause (extension ==? ".lhs") i
        then let p = evalClause filePath i in t++[p]
        else t

