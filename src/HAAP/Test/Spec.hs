{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Spec@ plugin to run test specifications.
-}


{-# LANGUAGE DeriveGeneric, StandaloneDeriving, TypeOperators, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, EmptyDataDecls, FlexibleContexts, TypeFamilies, OverloadedStrings, GADTs, ScopedTypeVariables, DeriveTraversable #-}

module HAAP.Test.Spec where

import HAAP.Core
import HAAP.Utils
import HAAP.IO
import HAAP.Log
import HAAP.Pretty
import HAAP.Shelly
import HAAP.Plugin

import Test.QuickCheck.Property as QuickCheck hiding (Result)
import Test.QuickCheck as QuickCheck
import Test.QuickCheck.Gen as QuickCheck
import Test.QuickCheck.Random as QuickCheck
import Test.QuickCheck.Monadic as QuickCheck
import Test.Hspec hiding (runIO,Spec)
import qualified Test.Hspec as Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Formatters
import Test.Hspec.Contrib.HUnit
import Test.HUnit as HUnit hiding (State(..))
import qualified Test.HUnit.Lang as HUnit
import qualified Test.HUnit.Base as HUnit

import Control.Monad
import Control.Monad.State (State(..))
import qualified Control.Monad.State as State
import Control.Monad.Reader (Reader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Except
import Control.Exception
import Control.DeepSeq

import System.IO
import System.Environment
import System.FilePath
import System.IO.Unsafe

import Data.List
import Data.Maybe
import Data.Traversable
import Data.Typeable
import Data.Knob as Knob
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.CallStack
import Data.Default

import Control.Monad.Reader

import Text.Read

import Safe

data Spec

instance NFData Result where
    rnf = const () -- since this is a quickcheck result there is really no laziness issue, i.e., the test has been executed
    
instance HaapPlugin Spec where
    type PluginI Spec = HaapSpecArgs
    type PluginT Spec = ReaderT HaapSpecArgs
    type PluginO Spec = ()
    type PluginK Spec t m = ()
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . unComposeT) m
        return (x,())

useSpec :: (HaapStack t m,PluginK Spec t m) => (PluginI Spec) -> Haap (PluginT Spec :..: t) m a -> Haap t m a
useSpec args m = usePlugin_ (return args) m

instance HaapMonad m => HasPlugin Spec (ReaderT HaapSpecArgs) m where
    liftPlugin = id
instance (HaapStack t2 m,HaapPluginT (ReaderT HaapSpecArgs) m (t2 m)) => HasPlugin Spec (ComposeT (ReaderT HaapSpecArgs) t2) m where
    liftPlugin m = ComposeT $ hoistPluginT liftStack m

data HaapSpecArgs = HaapSpecArgs
    { specMode :: HaapSpecMode
    , specQuickCheckMaxSuccess :: Maybe Int
    , specIO :: IOArgs
    }

instance Default HaapSpecArgs where
    def = defaultHaapSpecArgs

defaultHaapSpecArgs :: HaapSpecArgs
defaultHaapSpecArgs = HaapSpecArgs HaapSpecQuickCheck Nothing def

data HaapSpecMode = HaapSpecQuickCheck | HaapSpecHUnit

instance Out HUnit.FailureReason where
    docPrec i x = doc x
    doc err = text (HUnit.formatFailureReason err)

instance Out FailureReason where
    docPrec i x = doc x
    doc NoReason = text "no reason"
    doc (Reason str) = text "reason" <+> text str
    doc (ExpectedButGot msg x y) = doc msg $+$ nest 4 (text "expected:" <+> text x $+$ text "got:" <+> text y)

runSpec :: (MonadIO m,HasPlugin Spec t m) => HaapSpec -> Haap t m HaapTestTableRes
runSpec spec = do
    logEvent "Running Haap Specification"
    args <- liftHaap $ liftPluginProxy (Proxy::Proxy Spec) $ Reader.ask
    let tests = haapSpec (specIO args) (specMode args) spec
    runHaapTestTable args tests

-- generates a list of tests
bounded :: (Out a,Show a) => String -> [a] -> (a -> HaapSpec) -> HaapSpec
bounded = HaapSpecBounded

-- generates a random test (receives a set of seeds for grading)
unbounded :: (Out a,Show a) => String -> [Int] -> Gen a -> (a -> HaapSpec) -> HaapSpec
unbounded = HaapSpecUnbounded

testBool :: Bool -> HaapSpec
testBool x = testBoolIO (return x)

testEqual :: (NFData a,Eq a,OutIO a) => a -> a -> HaapSpec
testEqual x y = testEqualIO (return x) (return y)

testEqualWith :: (NFData a,OutIO a) => (a -> a -> Bool) -> a -> a -> HaapSpec
testEqualWith eq x y = testEqualIOWith eq (return x) (return y)

testMessage :: String -> HaapSpec
testMessage x = testMessageIO (return x)

testBoolIO :: IO Bool -> HaapSpec
testBoolIO = HaapSpecTestBool

testMaybeIO :: (NFData a,OutIO a) => IO (Maybe a) -> HaapSpec
testMaybeIO = HaapSpecTestMaybe

testEqualIO :: (NFData a,Eq a,OutIO a) => IO a -> IO a -> HaapSpec
testEqualIO = HaapSpecTestEqual (==)

testEqualIOWith :: (NFData a,OutIO a) => (a -> a -> Bool) -> IO a -> IO a -> HaapSpec
testEqualIOWith eq iox ioy = HaapSpecTestEqual eq iox ioy

testMessageIO :: IO String -> HaapSpec
testMessageIO = HaapSpecTestMessage

testResult :: Result -> HaapSpec
testResult = HaapSpecTestResult . return

testResultIO :: IO Result -> HaapSpec
testResultIO = HaapSpecTestResult

data HaapSpec where
     HaapSpecBounded :: (Show a,Out a) => String -> [a] -> (a -> HaapSpec) -> HaapSpec
     HaapSpecUnbounded :: (Show a,Out a) => String -> [Int] -> Gen a -> (a -> HaapSpec) -> HaapSpec
     HaapSpecTestBool :: IO Bool -> HaapSpec
     HaapSpecTestMaybe :: (NFData a,OutIO a) => IO (Maybe a) -> HaapSpec
     HaapSpecTestEqual :: (NFData a,OutIO a) => (a -> a -> Bool) -> IO a -> IO a -> HaapSpec
     HaapSpecTestMessage :: IO String -> HaapSpec
     HaapSpecTestResult :: IO Result -> HaapSpec -- the result of running a quickCheck property

data HaapTestTable a = HaapTestTable
    { haapTestTableHeader :: [String] -- table header
    , haapTestTableRows   :: [([String],a)] -- table of tests
    }
  deriving (Functor,Traversable,Foldable,Show,Read,Typeable)

haapTableResOksPercentage :: HaapTestTableRes -> Float
haapTableResOksPercentage t = (fromIntegral (length oks) / fromIntegral (length rows)) * 100
    where
    rows = haapTestTableRows t
    oks = filter ((==HaapTestOk) . snd) rows

haapTableResOKsKOs :: HaapTestTableRes -> (Int,Int)
haapTableResOKsKOs t = (length oks,length kos)
    where
    rows = haapTestTableRows t
    (oks,kos) = partition ((==HaapTestOk) . snd) rows

haapTableResPercentage :: HaapTestTableRes -> Float
haapTableResPercentage t = oks
    where
    rows = haapTestTableRows t
    oks = averageList $ map (haapTestResPercentage . snd) rows

type HaapTestTableRes = HaapTestTable HaapTestRes

data HaapTestRes
    = HaapTestOk
    | HaapTestError String
    | HaapTestMessage String
  deriving (Eq,Ord,Read,Show)

instance Out HaapTestRes where
    docPrec i x = doc x
    doc HaapTestOk = text "OK"
    doc (HaapTestError msg) = text msg
    doc (HaapTestMessage msg) = text msg

haapTestResPercentage :: HaapTestRes -> Float
haapTestResPercentage HaapTestOk = 100
haapTestResPercentage (HaapTestError _) = 0
haapTestResPercentage (HaapTestMessage str) = read str

haapNewExample :: State Int Int
haapNewExample = do
    i <- State.get
    State.modify succ
    return $ i

haapTestRes :: Either SomeException FailureReason -> HaapTestRes
haapTestRes (Left some) = case fromException some of
    Nothing -> HaapTestError $ pretty some
    Just (HaapSpecMessage msg) -> HaapTestMessage msg
haapTestRes (Right msg) = HaapTestError $ pretty msg

runHaapTestTable :: (MonadIO m,HasPlugin Spec t m) => HaapSpecArgs -> HaapTestTable (Int,Hspec.Spec) -> Haap t m HaapTestTableRes
runHaapTestTable args tests = orDo (\e -> return $ fmapDefault (const $ HaapTestError $ pretty e) tests) $ do
    forM tests $ \(i,spec) -> runHaapTest args i spec
    
runHaapTest :: (MonadIO m,HasPlugin Spec t m) => HaapSpecArgs -> Int -> Hspec.Spec -> Haap t m HaapTestRes
runHaapTest args ex test = orDo (\e -> return $ HaapTestError $ pretty e) $ do
    let ioargs = specIO args
    outknob <- addMessageToError ("initializing test knob" ++ show ex) $ runBaseIOWith (ioargs) $ newKnob (B.pack [])
    outhandle <- addMessageToError ("initializing test handle" ++ show ex) $ runBaseIOWith (ioargs) $ newFileHandle outknob "knob" WriteMode
    let formatter = silent
            { exampleSucceeded = \parents name -> write $ "HaapTestOk"
            , exampleFailed = \parents name err -> write $ show (haapTestRes $ Right err)
            }
    let cfg = defaultConfig
                { configQuickCheckMaxSuccess = specQuickCheckMaxSuccess args
                , configFormatter = Just formatter
                , configOutputFile = Left outhandle
                }
    let spec = describe (show ex) test
    ignoreError $ runBaseIOWith' (ioargs) $ withArgs [] $ hspecWith cfg spec
    ignoreError $ runBaseIOWith' (ioargs) $ hClose outhandle
    outbstr <- orLogError $ runBaseIO' $ Knob.getContents outknob

    let outstr = B8.unpack outbstr
    res <- case readMaybe outstr :: Maybe HaapTestRes of
        Nothing -> throwError $ HaapException $ "failed to parse hspec output: " ++ outstr
        Just res -> return res
    return res

haapSpec :: IOArgs -> HaapSpecMode -> HaapSpec -> HaapTestTable (Int,Hspec.Spec)
haapSpec ioargs mode s = State.evalState (haapSpec' mode s) 0
    where 
    appendTable (HaapTestTable h1 t1) (HaapTestTable h2 t2) | h1 == h2 = HaapTestTable h1 (t1 ++ t2)
    concatTable [] = HaapTestTable [] []
    concatTable xs = foldr1 appendTable xs
        
    haapSpec' :: HaapSpecMode -> HaapSpec -> State Int (HaapTestTable (Int,Hspec.Spec))
    haapSpec' mode (HaapSpecBounded n xs f) = do
        let add x = do
            HaapTestTable h t <- haapSpec' mode (f x)
            return $ HaapTestTable (n:h) (map (mapFst (show x :)) t)
        ys <- mapM add xs
        return $ concatTable ys
    haapSpec' HaapSpecQuickCheck spec@(HaapSpecUnbounded n seeds g f) = do
        ex <- haapNewExample
        let ns = unsafePerformIO $ haapSpecNames spec
        return $ HaapTestTable ns [(replicate (length ns) "randomly sampled",(ex,it (show ex) $ forAll g f))]
    haapSpec' HaapSpecHUnit (HaapSpecUnbounded n seeds g f) = do
        let mkArg i = unGen g (mkQCGen i) i
        let xs = map mkArg seeds
        haapSpec' HaapSpecHUnit (HaapSpecBounded n xs f)
    haapSpec' mode (HaapSpecTestBool io) = do
        ex <- haapNewExample
        let s = fromHUnitTest $ TestLabel (show ex) $ TestCase $ do
            b <- runSpecIO ioargs "test" io
            assertBool "Boolean assertion failed" b
        return $ HaapTestTable [] [([],(ex,describe (show ex) s))]
    haapSpec' mode (HaapSpecTestMaybe io) = do
        ex <- haapNewExample
        let s = fromHUnitTest $ TestLabel (show ex) $ TestCase $ do
            b <- runSpecIO ioargs "test" io
            assertMaybe "Boolean assertion failed" b
        return $ HaapTestTable [] [([],(ex,describe (show ex) s))]
    haapSpec' mode (HaapSpecTestEqual eq iox ioy) = do
        ex <- haapNewExample
        let s = fromHUnitTest $ TestLabel (show ex) $ TestCase $ do
            x <- runSpecIO ioargs "oracle" iox
            y <- runSpecIO ioargs "solution" ioy
            assertEqualWith ("Equality assertion failed: ") eq x y
        return $ HaapTestTable [] [([],(ex,describe (show ex) s))]
    haapSpec' mode (HaapSpecTestMessage io) = do
        ex <- haapNewExample
        let s = fromHUnitTest $ TestLabel (show ex) $ TestCase $ do
            msg <- runSpecIO ioargs "test" io 
            throw $ HaapSpecMessage msg
        return $ HaapTestTable [] [([],(ex,describe (show ex) s))]
    haapSpec' mode (HaapSpecTestResult io) = do
        ex <- haapNewExample
        let s = fromHUnitTest $ TestLabel (show ex) $ TestCase $ do
            res <- runSpecIO ioargs "test" io 
            assertResult res
        return $ HaapTestTable [] [([],(ex,describe (show ex) s))]

data HaapSpecMessage = HaapSpecMessage String
  deriving Show

instance Exception HaapSpecMessage where
    displayException (HaapSpecMessage msg) = msg

runSpecIO :: NFData a => IOArgs -> String -> IO a -> IO a
runSpecIO ioargs side io = do
    catch (forceM io) (\(e::SomeException) -> error $ side ++ " failed: " ++ pretty e)

instance QuickCheck.Testable HaapSpec where
    property = haapSpecProperty defaultIOArgs

haapSpecNames :: HaapSpec -> IO [String]
haapSpecNames (HaapSpecBounded n xs f) = do
    ns <- mapM (haapSpecNames . f) (headMay xs)
    return (n:maybe [] id ns)
haapSpecNames (HaapSpecUnbounded n _ g f) = do
    x <- generate g
    ns <- haapSpecNames $ f x
    return (n:ns)
haapSpecNames (HaapSpecTestBool io) = return []
haapSpecNames (HaapSpecTestMaybe io) = return []
haapSpecNames (HaapSpecTestEqual eq iox ioy) = return []
haapSpecNames (HaapSpecTestMessage io) = return []
haapSpecNames (HaapSpecTestResult io) = return []

haapSpecProperty :: IOArgs -> HaapSpec -> Property
haapSpecProperty ioargs (HaapSpecBounded n xs f) = conjoin $ map (haapSpecProperty ioargs . f) xs
haapSpecProperty ioargs (HaapSpecUnbounded n _ g f) = forAll g f
haapSpecProperty ioargs (HaapSpecTestBool io) = counterexample "Boolean assertion failed" $ monadicIO $ do
    b <- run $ runSpecIO ioargs "test" io
    QuickCheck.assert b
haapSpecProperty ioargs (HaapSpecTestMaybe io) = counterexample "Boolean assertion failed" $ monadicIO $ do
    b <- run $ runSpecIO ioargs "test" io
    QuickCheck.assert (isNothing b)
haapSpecProperty ioargs (HaapSpecTestEqual eq iox ioy) = monadicIO $ do
    x <- run $ runSpecIO ioargs "oracle" iox
    y <- run $ runSpecIO ioargs "solution" ioy
    px <- run $ runSpecIO ioargs "oracle" $ prettyIO x
    py <- run $ runSpecIO ioargs "solution" $ prettyIO y
    unless (x `eq` y) $ fail ("Equality assertion failed: expected...\n"++px ++ "\n...but got...\n"++ py)
    QuickCheck.assert (x `eq` y)
haapSpecProperty ioargs (HaapSpecTestMessage io) = monadicIO $ do
    msg <- run $ runSpecIO ioargs "test" io
    throw $ HaapSpecMessage msg
    return ()
haapSpecProperty ioargs (HaapSpecTestResult io) = monadicIO $ do
    res <- run $ runSpecIO ioargs "test" io
    unless (isSuccessResult res) $ fail $ show res
    QuickCheck.assert (isSuccessResult res)

-- HUnit code

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

assertMaybe :: (HasCallStack, OutIO a)
                              => String -- ^ The message prefix
                              -> (Maybe a)      -- ^ The result with maybe the unexpected result
                              -> Assertion
assertMaybe preface gotten = case gotten of
    Nothing -> return ()
    Just actual -> do
        prefaceMsg <- if null preface then return Nothing else return $ Just preface
        actualMsg <- prettyIO actual
        throwIO $ HUnit.HUnitFailure location $ HUnit.ExpectedButGot prefaceMsg "" actualMsg

assertEqualWith :: (HasCallStack, OutIO a)
                              => String -- ^ The message prefix
                              -> (a -> a -> Bool)
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqualWith preface eq expected actual = unless (actual `eq` expected) $ do
    prefaceMsg <- if null preface then return Nothing else return $ Just preface
    expectedMsg <- prettyIO expected
    actualMsg <- prettyIO actual
    throwIO $ HUnit.HUnitFailure location $ HUnit.ExpectedButGot prefaceMsg expectedMsg actualMsg

assertResult :: (HasCallStack) => Result -> Assertion
assertResult res = unless (isSuccessResult res) $ do
    throwIO $ HUnit.HUnitFailure location $ HUnit.Reason $ show res
    
c (Success {}) = True
isSuccessResult _ = False


