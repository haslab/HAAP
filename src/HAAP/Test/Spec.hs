{-# LANGUAGE GADTs, ScopedTypeVariables, DeriveTraversable #-}

module HAAP.Test.Spec where

import HAAP.Core
import HAAP.Utils
import HAAP.IO
import HAAP.Log
import HAAP.Pretty

import Test.QuickCheck as QuickCheck
import Test.QuickCheck.Gen as QuickCheck
import Test.QuickCheck.Random as QuickCheck
import Test.QuickCheck.Monadic as QuickCheck
import Test.Hspec hiding (runIO)
import Test.Hspec.Core.Runner
import Test.Hspec.Formatters
import Test.Hspec.Contrib.HUnit
import Test.HUnit as HUnit hiding (State(..))
import qualified Test.HUnit.Lang as HUnit

import Control.Monad
import Control.Monad.State (State(..))
import qualified Control.Monad.State as State
import Control.Monad.Reader (Reader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Except

import System.IO
import System.Environment
import System.FilePath
import System.IO.Unsafe

import Data.Traversable
import Data.Typeable
import Data.Knob as Knob
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Text.Read

import Safe

data HaapSpecArgs = HaapSpecArgs
    { specMode :: HaapSpecMode
    , specQuickCheckMaxSuccess :: Maybe Int
    }

data HaapSpecMode = HaapSpecQuickCheck | HaapSpecHUnit

instance Out HUnit.FailureReason where
    docPrec i x = doc x
    doc err = text (HUnit.formatFailureReason err)

instance Out FailureReason where
    docPrec i x = doc x
    doc NoReason = text "no reason"
    doc (Reason str) = text "reason" <+> text str
    doc (ExpectedButGot msg x y) = doc msg $+$ nest 4 (text "expected:" <+> text x $+$ text "got:" <+> text y)

runSpec :: HaapMonad m => HaapSpec -> Haap p HaapSpecArgs db m HaapTestTableRes
runSpec = runSpecWith id

runSpecWith :: HaapMonad m => (args -> HaapSpecArgs) -> HaapSpec -> Haap p args db m HaapTestTableRes
runSpecWith getArgs spec = do
    logEvent "Running Haap Specification"
    args <- liftM getArgs $ Reader.ask
    let tests = haapSpec (specMode args) spec
    runHaapTestTable args tests

-- generates a list of tests
bounded :: (Show a) => String -> [a] -> (a -> HaapSpec) -> HaapSpec
bounded = HaapSpecBounded

-- generates a random test (receives a set of seeds for grading)
unbounded :: Show a => String -> [Int] -> Gen a -> (a -> HaapSpec) -> HaapSpec
unbounded = HaapSpecUnbounded

testBool :: IO Bool -> HaapSpec
testBool = HaapSpecTestBool

testEqual :: (Eq a,Show a) => IO (a,a) -> HaapSpec
testEqual = HaapSpecTestEqual

data HaapSpec where
     HaapSpecBounded :: Show a => String -> [a] -> (a -> HaapSpec) -> HaapSpec
     HaapSpecUnbounded :: Show a => String -> [Int] -> Gen a -> (a -> HaapSpec) -> HaapSpec
     HaapSpecTestBool :: IO Bool -> HaapSpec
     HaapSpecTestEqual :: (Show a,Eq a) => IO (a,a) -> HaapSpec

data HaapTestTable a = HaapTestTable
    { haapTestTableHeader :: [String] -- table header
    , haapTestTableRows   :: [([String],a)] -- table of tests
    }
  deriving (Functor,Traversable,Foldable,Show,Read,Typeable)

type HaapTestTableRes = HaapTestTable HaapTestRes

type HaapTestRes = Maybe HaapException

haapNewExample :: State Int Int
haapNewExample = do
    i <- State.get
    State.modify succ
    return i

runHaapTestTable :: HaapMonad m => HaapSpecArgs -> HaapTestTable (Int,Spec) -> Haap p args db m HaapTestTableRes
runHaapTestTable args tests = orDo (\e -> return $ fmapDefault (const $ Just e) tests) $ do
    outknob <- runIO $ newKnob (B.pack [])
    outhandle <- runIO $ newFileHandle outknob "knob" WriteMode
    runIO $ hPutStr outhandle "[(0,Nothing)"
    let formatter = silent
            { exampleSucceeded = \(parents,name) -> write $ ",(" ++ name ++ "," ++ "Nothing" ++ ")"
            , exampleFailed = \(parents,name) err -> write $ ",(" ++ name ++ "," ++ "Just "  ++ show (either pretty pretty err) ++ ")"
            }
    let cfg = defaultConfig
                { configQuickCheckMaxSuccess = specQuickCheckMaxSuccess args
                , configFormatter = Just formatter
                , configOutputFile = Left outhandle
                }
    let spec = forM_ tests $ \(ex,test) -> describe (show ex) test
    ignoreError $ runIO' $ withArgs [] $ hspecWith cfg spec
    runIO $ hPutStr outhandle "]"
    runIO $ hClose outhandle
    
    outbstr <- runIO' $ Knob.getContents outknob
    let outstr = B8.unpack outbstr
    xs <- case readMaybe outstr :: Maybe [(Int,Maybe String)] of
        Nothing -> throwError $ HaapException $ "failed to parse hspec output: " ++ outstr
        Just xs -> return $ tail xs
    let readTest (i,x) = case lookup i xs of
                            Just xres -> fmap HaapException xres
                            Nothing -> Just $ HaapException $ "example name not found " ++ show i ++ " in " ++ show (map fst xs)
    let res = fmapDefault readTest tests
    return res

haapSpec :: HaapSpecMode -> HaapSpec -> HaapTestTable (Int,Spec)
haapSpec mode s = State.evalState (haapSpec' mode s) 0
    where 
    appendTable (HaapTestTable h1 t1) (HaapTestTable h2 t2) | h1 == h2 = HaapTestTable h1 (t1 ++ t2)
    concatTable [] = HaapTestTable [] []
    concatTable xs = foldr1 appendTable xs
        
    haapSpec' :: HaapSpecMode -> HaapSpec -> State Int (HaapTestTable (Int,Spec))
    haapSpec' mode (HaapSpecBounded n xs f) = do
        let add x = do
            HaapTestTable h t <- haapSpec' mode (f x)
            return $ HaapTestTable (n:h) (map (mapFst (show x :)) t)
        ys <- mapM add xs
        return $ concatTable ys
    haapSpec' HaapSpecQuickCheck spec@(HaapSpecUnbounded n seeds g f) = do
        ex <- haapNewExample
        let ns = unsafePerformIO $ haapSpecNames spec
        return $ HaapTestTable ns [(replicate (length ns) "-",(ex,it (show ex) $ forAll g f))]
    haapSpec' HaapSpecHUnit (HaapSpecUnbounded n seeds g f) = do
        let mkArg i = unGen g (mkQCGen i) i
        let xs = map mkArg seeds
        haapSpec' HaapSpecHUnit (HaapSpecBounded n xs f)
    haapSpec' mode (HaapSpecTestBool io) = do
        ex <- haapNewExample
        let s = fromHUnitTest $ TestLabel (show ex) $ TestCase $ do
            b <- io
            assertBool "Boolean assertion failed" b
        return $ HaapTestTable [] [([],(ex,describe (show ex) s))]
    haapSpec' mode (HaapSpecTestEqual io) = do
        ex <- haapNewExample
        let s = fromHUnitTest $ TestLabel (show ex) $ TestCase $ do
            (x,y) <- io
            assertEqual ("Equality assertion failed: expected...\n"++show x ++ "\n...but got...\n"++ show y) x y
        return $ HaapTestTable [] [([],(ex,describe (show ex) s))]

instance QuickCheck.Testable HaapSpec where
    property = haapSpecProperty

haapSpecNames :: HaapSpec -> IO [String]
haapSpecNames (HaapSpecBounded n xs f) = do
    ns <- mapM (haapSpecNames . f) (headMay xs)
    return (n:maybe [] id ns)
haapSpecNames (HaapSpecUnbounded n _ g f) = do
    x <- generate g
    ns <- haapSpecNames $ f x
    return (n:ns)
haapSpecNames (HaapSpecTestBool io) = return []
haapSpecNames (HaapSpecTestEqual io) = return []

haapSpecProperty :: HaapSpec -> Property
haapSpecProperty (HaapSpecBounded n xs f) = conjoin $ map (haapSpecProperty . f) xs
haapSpecProperty (HaapSpecUnbounded n _ g f) = forAll g f
haapSpecProperty (HaapSpecTestBool io) = counterexample "Boolean assertion failed" $ monadicIO $ do
    b <- run io
    QuickCheck.assert b
haapSpecProperty (HaapSpecTestEqual io) = monadicIO $ do
    (x,y) <- run io
    unless (x==y) $ fail ("Equality assertion failed: expected...\n"++show x ++ "\n...but got...\n"++ show y)
    QuickCheck.assert (x==y)
