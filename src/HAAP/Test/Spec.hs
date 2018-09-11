{-
HAAP: Haskell Automated Assessment Platform

This module provides the @Spec@ plugin to run test specifications.
-}


{-# LANGUAGE OverloadedStrings, DeriveGeneric, StandaloneDeriving, TypeOperators, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, EmptyDataDecls, FlexibleContexts, TypeFamilies, GADTs, ScopedTypeVariables, DeriveTraversable #-}

module HAAP.Test.Spec where

import HAAP.Core
import HAAP.Utils
import HAAP.IO
import HAAP.Log
import HAAP.Pretty
import HAAP.Shelly
import HAAP.Plugin

import Test.QuickCheck.Property as QuickCheck hiding (Result(..))
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
import Control.Monad.Base
import Control.Monad.Trans.Compose
import Control.Monad.State (State(..))
import qualified Control.Monad.State as State
import Control.Monad.Reader (Reader(..))
import qualified Control.Monad.Reader as Reader
--import Control.Monad.Except
--import Control.Exception
import Control.DeepSeq
import Control.Monad.Reader
import Control.Exception.Safe
import qualified Control.Monad.Catch as C

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
import qualified Data.Text as T

import Text.Read hiding (lift)

import Safe

data Spec

instance C.MonadThrow m => C.MonadThrow (PropertyM m) where
    throwM e = run $ throw e

instance NFData Result where
    rnf = const () -- since this is a quickcheck result there is really no laziness issue, i.e., the test has been executed
    
instance HaapPlugin Spec where
    type PluginI Spec = HaapSpecArgs
    type PluginT Spec = ReaderT HaapSpecArgs
    type PluginO Spec = ()
    type PluginK Spec t m = ()
    
    usePlugin getArgs m = do
        args <- getArgs
        x <- mapHaapMonad (flip Reader.runReaderT args . getComposeT) m
        return (x,())

useSpec :: (HaapStack t m,PluginK Spec t m) => (PluginI Spec) -> Haap (PluginT Spec :..: t) m a -> Haap t m a
useSpec args m = usePlugin_ (return args) m

instance (MonadCatch m) => HasPlugin Spec (ReaderT HaapSpecArgs) m where
    liftPlugin = id
instance (HaapStack t2 m) => HasPlugin Spec (ComposeT (ReaderT HaapSpecArgs) t2) m where
    liftPlugin m = ComposeT $ hoist' lift m

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

instance Pretty HUnit.FailureReason where
    pretty err = string (HUnit.formatFailureReason err)

instance Pretty FailureReason where
    pretty NoReason = text "no reason"
    pretty (Reason str) = text "reason" <+> string str
    pretty (ExpectedButGot msg x y) = pretty msg $+$ nest 4 (text "expected:" <+> string x $+$ text "got:" <+> string y)

instance Pretty Result where
    pretty res = case res of
        Success {} -> "OK"
        GaveUp {} -> string "gave up:" <+> string (output res)
        Failure {} -> string "failed:" <+> string (reason res)
                  $+$ maybe mempty (\e -> string "exception:" <+> pretty e) (theException res)
                  $+$ string "output:" <+> string (output res)
                  $+$ string "test case:" <+> hsep (map string $ failingLabels res) <+> hsep (map string $ failingTestCase res) 
        NoExpectedFailure {} -> string "failed:" <+> string (output res)

runSpec :: (MonadIO m,HasPlugin Spec t m) => HaapSpec -> Haap t m HaapTestTableRes
runSpec spec = do
    logEvent "Running Haap Specification"
    args <- liftHaap $ liftPluginProxy (Proxy::Proxy Spec) $ Reader.ask
    let tests = haapSpec (specIO args) (specMode args) spec
    runHaapTestTable args tests

-- generates a list of tests
bounded :: (Pretty a) => T.Text -> [a] -> (a -> HaapSpec) -> HaapSpec
bounded = HaapSpecBounded

-- generates a random test (receives a set of seeds for grading)
unbounded :: (Pretty a) => T.Text -> [Int] -> Gen a -> (a -> HaapSpec) -> HaapSpec
unbounded = HaapSpecUnbounded

testBool :: Bool -> HaapSpec
testBool x = testBoolIO (return x)

testMaybe :: (NFData a,PrettyIO a) => Maybe a -> HaapSpec
testMaybe x = testMaybeIO (return x)

testEqual :: (NFData a,Eq a,PrettyIO a) => a -> a -> HaapSpec
testEqual x y = testEqualIO (return x) (return y)

testEqualWith :: (NFData a,PrettyIO a) => (a -> a -> Bool) -> a -> a -> HaapSpec
testEqualWith eq x y = testEqualIOWith eq (return x) (return y)

testMessage :: T.Text -> HaapSpec
testMessage x = testMessageIO (return x)

testBoolIO :: IO Bool -> HaapSpec
testBoolIO = HaapSpecTestBool

testMaybeIO :: (NFData a,PrettyIO a) => IO (Maybe a) -> HaapSpec
testMaybeIO = HaapSpecTestMaybe

testEqualIO :: (NFData a,Eq a,PrettyIO a) => IO a -> IO a -> HaapSpec
testEqualIO = HaapSpecTestEqual (==)

testEqualIOWith :: (NFData a,PrettyIO a) => (a -> a -> Bool) -> IO a -> IO a -> HaapSpec
testEqualIOWith eq iox ioy = HaapSpecTestEqual eq iox ioy

testMessageIO :: IO T.Text -> HaapSpec
testMessageIO = HaapSpecTestMessage

testResult :: Result -> HaapSpec
testResult = HaapSpecTestResult . return

testResultIO :: IO Result -> HaapSpec
testResultIO = HaapSpecTestResult

data HaapSpec where
     HaapSpecBounded :: (Pretty a) => T.Text -> [a] -> (a -> HaapSpec) -> HaapSpec
     HaapSpecUnbounded :: (Pretty a) => T.Text -> [Int] -> Gen a -> (a -> HaapSpec) -> HaapSpec
     HaapSpecTestBool :: IO Bool -> HaapSpec
     HaapSpecTestMaybe :: (NFData a,PrettyIO a) => IO (Maybe a) -> HaapSpec
     HaapSpecTestEqual :: (NFData a,PrettyIO a) => (a -> a -> Bool) -> IO a -> IO a -> HaapSpec
     HaapSpecTestMessage :: IO T.Text -> HaapSpec
     HaapSpecTestResult :: IO Result -> HaapSpec -- the result of running a quickCheck property

data HaapTestTable a = HaapTestTable
    { haapTestTableHeader :: [T.Text] -- table header
    , haapTestTableRows   :: [([T.Text],a)] -- table of tests
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
    | HaapTestError T.Text
    | HaapTestMessage T.Text
  deriving (Eq,Ord,Read,Show)

instance Pretty HaapTestRes where
    pretty HaapTestOk = text "OK"
    pretty (HaapTestError msg) = text msg
    pretty (HaapTestMessage msg) = text msg

haapTestResPercentage :: HaapTestRes -> Float
haapTestResPercentage HaapTestOk = 100
haapTestResPercentage (HaapTestError _) = 0
haapTestResPercentage (HaapTestMessage str) = read $ T.unpack str

haapNewExample :: State Int Int
haapNewExample = do
    i <- State.get
    State.modify succ
    return $ i

haapTestRes :: Either SomeException FailureReason -> HaapTestRes
haapTestRes (Left some) = case fromException some of
    Nothing -> HaapTestError $ prettyText some
    Just (HaapSpecMessage msg) -> HaapTestMessage msg
haapTestRes (Right msg) = HaapTestError $ prettyText msg

runHaapTestTable :: (MonadIO m,HasPlugin Spec t m) => HaapSpecArgs -> HaapTestTable (Int,Hspec.Spec) -> Haap t m HaapTestTableRes
runHaapTestTable args tests = orDo (\e -> return $ fmapDefault (const $ HaapTestError $ prettyText e) tests) $ do
    forM tests $ \(i,spec) -> runHaapTest args i spec
    
runHaapTest :: (MonadIO m,HasPlugin Spec t m) => HaapSpecArgs -> Int -> Hspec.Spec -> Haap t m HaapTestRes
runHaapTest args ex test = orDo (\e -> return $ HaapTestError $ prettyText e) $ do
    let ioargs = specIO args
    outknob <- addMessageToError ("initializing test knob" <> prettyText ex) $ runBaseIOWith (ioargs) $ newKnob (B.pack [])
    outhandle <- addMessageToError ("initializing test handle" <> prettyText ex) $ runBaseIOWith (ioargs) $ newFileHandle outknob "knob" WriteMode
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
        Nothing -> throw $ HaapException $ T.pack $ "failed to parse hspec output: " ++ outstr
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
            return $ HaapTestTable (n:h) (map (mapFst (prettyText x :)) t)
        ys <- mapM add xs
        return $ concatTable ys
    haapSpec' HaapSpecQuickCheck spec@(HaapSpecUnbounded n seeds g f) = do
        ex <- haapNewExample
        let ns = unsafePerformIO $ haapSpecNames spec
        return $ HaapTestTable ns [(replicate (length ns) "randomly sampled",(ex,it (show ex) $ forAllShow g prettyString f))]
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
            assertMaybe "Maybe assertion failed" b
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

data HaapSpecMessage = HaapSpecMessage T.Text
  deriving Show

instance Exception HaapSpecMessage where
    displayException (HaapSpecMessage msg) = prettyString msg

runSpecIO :: NFData a => IOArgs -> T.Text -> IO a -> IO a
runSpecIO ioargs side io = do
    catch (forceM io) (\(e::SomeException) -> error $ T.unpack side ++ " failed: " ++ prettyString e)

instance QuickCheck.Testable HaapSpec where
    property = haapSpecProperty defaultIOArgs

haapSpecNames :: HaapSpec -> IO [T.Text]
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
haapSpecProperty ioargs (HaapSpecUnbounded n _ g f) = forAllShow g prettyString f
haapSpecProperty ioargs (HaapSpecTestBool io) = counterexample "Boolean assertion failed" $ monadicIO $ do
    b <- run $ runSpecIO ioargs "test" io
    QuickCheck.assert b
haapSpecProperty ioargs (HaapSpecTestMaybe io) = counterexample "Maybe assertion failed" $ monadicIO $ do
    b <- run $ runSpecIO ioargs "test" io
    unless (isNothing b) $ do
        pb <- run $ runSpecIO ioargs "solution" $ prettyStringIO (fromJust b)
        fail ("Maybe assertion failed: got...\n"++pb)
    QuickCheck.assert (isNothing b)
haapSpecProperty ioargs (HaapSpecTestEqual eq iox ioy) = monadicIO $ do
    x <- run $ runSpecIO ioargs "oracle" iox
    y <- run $ runSpecIO ioargs "solution" ioy
    px <- run $ runSpecIO ioargs "oracle" $ prettyStringIO x
    py <- run $ runSpecIO ioargs "solution" $ prettyStringIO y
    unless (x `eq` y) $ fail ("Equality assertion failed: expected...\n"++px ++ "\n...but got...\n"++ py)
    QuickCheck.assert (x `eq` y)
haapSpecProperty ioargs (HaapSpecTestMessage io) = monadicIO $ do
    msg <- run $ runSpecIO ioargs "test" io 
    throw $ HaapSpecMessage msg
    return ()
haapSpecProperty ioargs (HaapSpecTestResult io) = monadicIO $ do
    res <- run $ runSpecIO ioargs "test" io
    unless (isSuccessResult res) $ fail $ prettyString res
    QuickCheck.assert (isSuccessResult res)

-- HUnit code

location :: HasCallStack => Maybe SrcLoc
location = case reverse callStack of
  (_, loc) : _ -> Just loc
  [] -> Nothing

assertMaybe :: (HasCallStack, PrettyIO a)
                              => String -- ^ The message prefix
                              -> (Maybe a)      -- ^ The result with maybe the unexpected result
                              -> Assertion
assertMaybe preface gotten = case gotten of
    Nothing -> return ()
    Just actual -> do
        actualMsg <- prettyStringIO actual
        throwIO $ HUnit.HUnitFailure location $ HUnit.Reason $ preface ++ ": got...\n" ++ actualMsg

assertEqualWith :: (HasCallStack, PrettyIO a)
                              => String -- ^ The message prefix
                              -> (a -> a -> Bool)
                              -> a      -- ^ The expected value
                              -> a      -- ^ The actual value
                              -> Assertion
assertEqualWith preface eq expected actual = unless (actual `eq` expected) $ do
    prefaceMsg <- if null preface then return Nothing else return $ Just preface
    expectedMsg <- prettyStringIO expected
    actualMsg <- prettyStringIO actual
    throwIO $ HUnit.HUnitFailure location $ HUnit.ExpectedButGot prefaceMsg expectedMsg actualMsg

assertResult :: (HasCallStack) => Result -> Assertion
assertResult res = unless (isSuccessResult res) $ do
    throwIO $ HUnit.HUnitFailure location $ HUnit.Reason $ prettyString res
    
isSuccessResult (Success {}) = True
isSuccessResult _ = False


