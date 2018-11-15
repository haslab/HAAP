{-
HAAP: Haskell Automated Assessment Platform

This module provides basic metrics of data type and higher-order functions usage in Haskell code.

-}

{-# LANGUAGE OverloadedStrings, TupleSections, DeriveGeneric, CPP #-}

module HAAP.Code.Analysis.Usage
    ( Usage(..),UsageArgs(..),runUsage,UsageReport(..),usageReport,runUsageReport
    ) where

import HAAP.IO
import HAAP.Core
import HAAP.Code.Haskell hiding (nameString,moduleName,moduleNameString)
import HAAP.Log
import HAAP.Plugin
import HAAP.Shelly

-- GHC imports
import GHC.Paths ( libdir )
import GHC.LanguageExtensions
import GHC hiding (def)
import Outputable
import DynFlags
import SrcLoc
import Bag
import Type
import NameSet
import FastString
import Name
import Module
import TcRnTypes
import HscTypes hiding (Usage)
import Var
import ConLike
import TyCon
import InstEnv

import Control.DeepSeq
import Data.Default
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad
import Data.Generics hiding (TyCon,tyConName)
import Control.Monad.IO.Class
import qualified GHC.Generics as GHC
import Data.Csv (encodeDefaultOrderedByName,DefaultOrdered(..),Record(..),ToNamedRecord(..),FromNamedRecord(..),(.:),(.=),header,namedRecord)
 
-- whether it is higher-order entity or not
type IsHO = Bool
-- whether it is a type synonym or a data/newtype
type IsTySyn = Bool
 
data Usage = Usage
    { definedFunctions :: Map String IsHO -- | functions defined in the analyzed modules
    , usedFunctions :: Map String IsHO -- | used functions imported from external modules
    , definedTypes :: Map String (IsTySyn,IsHO) -- types defined in the analyzed modules
    , usedTypes :: Map String IsHO -- used types imported from external modules
    , definedClasses :: Set String -- classes defined in the analyzed modules
    , usedClasses :: Set String -- used classes imported from external modules
    , instancesClasses :: Map String (String,IsHO) -- instances defined per class
}

data UsageReport = UsageReport
    { usageDefinedFunctionsNonHO :: Int
    , usageDefinedFunctionsHO :: Int
    , usageUsedFunctionsNonHO :: Int
    , usageUsedFunctionsHO :: Int
    , usageDefinedTypeSynonymsNonHO :: Int
    , usageDefinedTypeSynonymsHO :: Int
    , usageDefinedDataTypesNonHO :: Int
    , usageDefinedDataTypesHO :: Int
    , usageDefinedClasses :: Int
    , usageUsedClasses :: Int
    , usageDefinedClassInstancesNonHO :: Int
    , usageDefinedClassInstancesHO :: Int
    }
  deriving (Show,GHC.Generic)
    
instance NFData UsageReport where

instance DefaultOrdered UsageReport where
    headerOrder _ = header
        ["usageDefinedFunctionsNonHO"
        ,"usageDefinedFunctionsHO"
        ,"usageUsedFunctionsNonHO"
        ,"usageUsedFunctionsHO"
        ,"usageDefinedTypeSynonymsNonHO"
        ,"usageDefinedTypeSynonymsHO"
        ,"usageDefinedDataTypesNonHO"
        ,"usageDefinedDataTypesHO"
        ,"usageDefinedClasses"
        ,"usageUsedClasses"
        ,"usageDefinedClassInstancesNonHO"
        ,"usageDefinedClassInstancesHO"
        ]

instance ToNamedRecord UsageReport where
    toNamedRecord (UsageReport x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = namedRecord
        ["usageDefinedFunctionsNonHO" .= x1
        ,"usageDefinedFunctionsHO" .= x2
        ,"usageUsedFunctionsNonHO" .= x3
        ,"usageUsedFunctionsHO" .= x4
        ,"usageDefinedTypeSynonymsNonHO" .= x5
        ,"usageDefinedTypeSynonymsHO" .= x6
        ,"usageDefinedDataTypesNonHO" .= x7
        ,"usageDefinedDataTypesHO" .= x8
        ,"usageDefinedClasses" .= x9
        ,"usageUsedClasses" .= x10
        ,"usageDefinedClassInstancesNonHO" .= x11
        ,"usageDefinedClassInstancesHO" .= x12
        ]
instance FromNamedRecord UsageReport where
    parseNamedRecord m = UsageReport <$>
        m .: "usageDefinedFunctionsNonHO"
        <*> m .: "usageDefinedFunctionsHO"
        <*> m .: "usageUsedFunctionsNonHO"
        <*> m .: "usageUsedFunctionsHO"
        <*> m .: "usageDefinedTypeSynonymsNonHO"
        <*> m .: "usageDefinedTypeSynonymsHO"
        <*> m .: "usageDefinedDataTypesNonHO"
        <*> m .: "usageDefinedDataTypesHO"
        <*> m .: "usageDefinedClasses"
        <*> m .: "usageUsedClasses"
        <*> m .: "usageDefinedClassInstancesNonHO"
        <*> m .: "usageDefinedClassInstancesHO"

instance Default UsageReport where
    def = UsageReport (-1) (-1) (-1) (-1) (-1) (-1) (-1) (-1) (-1) (-1) (-1) (-1)

    
usageReport :: Usage -> UsageReport
usageReport u = UsageReport
    { usageDefinedFunctionsNonHO = Map.size defFunsNHO
    , usageDefinedFunctionsHO = Map.size defFunsHO
    , usageUsedFunctionsNonHO = Map.size useFunsNHO
    , usageUsedFunctionsHO = Map.size useFunsHO
    , usageDefinedTypeSynonymsNonHO = Map.size defsynsNHO
    , usageDefinedTypeSynonymsHO = Map.size defsynsHO
    , usageDefinedDataTypesNonHO = Map.size defdatasNHO
    , usageDefinedDataTypesHO = Map.size defdatasHO
    , usageDefinedClasses = Set.size $ definedClasses u
    , usageUsedClasses = Set.size $ usedClasses u
    , usageDefinedClassInstancesNonHO = Map.size instsHO
    , usageDefinedClassInstancesHO = Map.size instsNHO
    }
  where
    (defFunsHO,defFunsNHO) = Map.partition id $ definedFunctions u
    (useFunsHO,useFunsNHO) = Map.partition id $ usedFunctions u
    (defsyns,defdatas) = Map.partition fst $ definedTypes u
    (defsynsHO,defsynsNHO) = Map.partition snd defsyns
    (defdatasHO,defdatasNHO) = Map.partition snd defdatas 
    (instsHO,instsNHO) = Map.partition snd $ instancesClasses u

data UsageArgs = UsageArgs
    { usageFiles :: [(FilePath,String)] -- a list of Haskell files (with their module names) to analyze
    , usageIgnores :: String -> String -> Bool -- ignore predicate: receives name and module
    , usageImportPaths :: [FilePath] -- a list of additional import paths (similar to ghc's -i parameter)
    }

runUsageReport :: (MonadIO m,HaapStack t m) => UsageArgs -> Haap t m UsageReport
runUsageReport args = orLogDefault def (usageReport <$> runUsage args)
     
runUsage :: (MonadIO m,HaapStack t m) => UsageArgs -> Haap t m Usage
runUsage u = liftIO $ do
    
    -- compiler flags
    dflags <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        return $ (foldl xopt_set dflags [ImplicitPrelude]) { importPaths = usageImportPaths u }
        
--    let pretty = showSDoc dflags . ppr
    
    -- run ghc API
    (defthings,extthings,insts) <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            setSessionDynFlags dflags
            targets <- forM (usageFiles u) $ \(f,m) -> guessTarget f Nothing
            setTargets targets
            load LoadAllTargets
            tcenvs <- forM (usageFiles u) $ \(f,m) -> do
                modSum <- getModSummary $ mkModuleName m
                p <- parseModule modSum
                t <- typecheckModule p
                let (tcenv,_) = tm_internals_ t
                return tcenv
            
            let (defns,usedns) = foldl (flip defUses) (emptyNameSet,emptyNameSet) (map tcg_dus tcenvs)
            let extns = minusNameSet usedns defns
            
            let ignoreNameSet = filterNameSet $ \n -> not $ (usageIgnores u) (nameString n) (nameModuleString n)
            
            defthings <- catMaybes <$> forM (nameSetElemsStable $ ignoreNameSet defns) lookupName
            extthings <- catMaybes <$> forM (nameSetElemsStable $ ignoreNameSet extns) lookupName
            
            let insts = concatMap tcg_insts tcenvs
            
            return (defthings,extthings,insts)
    
    let (deffuns,deftys,defclss) = defThings defthings (Map.empty,Map.empty,Set.empty)
    let (usedfuns,usedtys,usedclss) = extThings extthings (Map.empty,Map.empty,Set.empty)
    
    let instclss = mkInsts insts Map.empty
    
    --putStrLn $ pretty defthings
    --putStrLn $ show deffuns
    --putStrLn $ show deftys
    --putStrLn $ show defclss
    --putStrLn $ pretty extthings
    --putStrLn $ show usedfuns
    --putStrLn $ show usedtys
    --putStrLn $ show usedclss
    --putStrLn $ show instclss
    
    return $ Usage deffuns usedfuns deftys usedtys defclss usedclss instclss
 
mkInsts :: [ClsInst] -> Map String (String,IsHO) -> Map String (String,IsHO)
mkInsts ds xs = foldl (flip mkInst) xs ds

mkInst :: ClsInst -> Map String (String,IsHO) -> Map String (String,IsHO)
mkInst i m = Map.insert n (showSDocUnsafe $ pprInstanceHdr i,isho) m
    where
    n = nameString $ is_cls_nm i
    isho = not $ null $ is_tvs i
 
nameString :: Name -> String
nameString = unpackFS . occNameFS . nameOccName

nameModuleString :: Name -> String
nameModuleString = moduleNameString . moduleName . nameModule
 
type ExtThings = (Map String IsHO,Map String IsHO,Set String)
 
extThings :: [TyThing] -> ExtThings -> ExtThings
extThings ds xs = foldl (flip extThing) xs ds

extThing :: TyThing -> ExtThings -> ExtThings
extThing (AnId fid) (usedfuns,usedtys,usedclss) = (Map.insertWith (||) n isho usedfuns,usedtys,usedclss)
    where
    n = nameString $ Var.varName fid
    isho = isHigherOrderType $ varType fid
extThing (AConLike con) (usedfuns,usedtys,usedclss) = (usedfuns,Map.insertWith (||) n isho usedtys,usedclss)
    where
    --n = nameString $ conLikeName con
    (args,_,_,_,_,_,ty) = conLikeFullSig con
    n = showSDocUnsafe (ppr ty)
    isho = isHigherOrderType ty
extThing (ATyCon con) (usedfuns,usedtys,usedclss)
    | isClassTyCon con = (usedfuns,usedtys,Set.insert n usedclss)
    | otherwise = (usedfuns,Map.insertWith (||) n isho usedtys,usedclss)
  where
    n = nameString $ tyConName con
    isho = not $ null $ tyConTyVars con
extThing thing useds = error $ "extThing: " ++ showSDocUnsafe (ppr thing)
 
type DefThings = (Map String IsHO,Map String (IsTySyn,IsHO),Set String)
 
defThings :: [TyThing] -> DefThings -> DefThings
defThings ds xs = foldl (flip defThing) xs ds

defThing :: TyThing -> DefThings -> DefThings
defThing (AnId fid) (deffuns,deftys,defclss) = (Map.insertWith (||) n isho deffuns,deftys,defclss)
    where
    n = nameString $ Var.varName fid
    isho = isHigherOrderType $ varType fid
defThing (ATyCon con) (deffuns,deftys,defclss)
    | isClassTyCon con = (deffuns,deftys,Set.insert n defclss)
    | isAlgTyCon con = (deffuns,Map.insertWith or2 n (False,isho) deftys,defclss)
    | isFunTyCon con || isTypeSynonymTyCon con = (deffuns,Map.insertWith or2 n (True,isho) deftys,defclss)
  where
    n = nameString $ tyConName con
    isho = not $ null $ tyConTyVars con
defThing (AConLike con) defs = defs
--defThing (AConLike con) (deffuns,deftys,defclss) = (deffuns,Map.insertWith or2 n (isTySynConLike con,isho) deftys,defclss)
--  where
--    n = nameString $ conLikeName con
--    (args,_,_,_,_,_,_) = conLikeFullSig con
--    isho = not $ null args
defThing thing defs = error $ "defThing: " ++ showSDocUnsafe (ppr thing)
 
isTySynConLike :: ConLike -> Bool
isTySynConLike (RealDataCon {}) = False
isTySynConLike (PatSynCon {}) = True
 
or2 (x1,x2) (y1,y2) = (x1 || y1,x2 || y2)
 
-- if it uses variables anywhere
isHigherOrderType :: Type -> Bool
isHigherOrderType x = everything (||) (mkQ False aux) x
    where
    aux :: Var -> Bool
    aux v = True
 
defUses :: DefUses -> (NameSet,NameSet) -> (NameSet,NameSet)
defUses ds xs = foldl (flip defUse) xs ds

defUse :: DefUse -> (NameSet,NameSet) -> (NameSet,NameSet)
defUse (ds1,us1) (ds2,us2) = (unionNameSet (maybe emptyNameSet id ds1) ds2,unionNameSet us1 us2)
 
--main :: IO ()
--main = do
--    let uargs = UsageArgs [("/Users/hpacheco/Desktop/HAAP/LI1-1819/svn/2018li1g201/src/Tarefa1_2018li1g201.hs","Tarefa1_2018li1g201")] (\n m -> m=="LI11819") ["/Users/hpacheco/Desktop/HAAP/LI1-1819/svn/2018li1g201/src/"]
--    u <- runUsage uargs
--    return ()
 

        




