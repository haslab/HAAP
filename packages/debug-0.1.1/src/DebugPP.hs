{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- GHC 8.0 doesn't know about COMPLETE pragmas

module DebugPP(main) where

import           Control.Monad
import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Yaml.Config
import           GHC.Generics
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Printf

usage :: String -> String
usage progName = unlines [
  "Usage: ",
  progName ++ " [FILENAME] [SOURCE] [DEST]",
  "  Instrument Haskell module for debugging from SOURCE (derived from FILENAME) and write",
  "  standard Haskell to DEST.",
  "  If no FILENAME, use SOURCE as the original name.",
  "  If no DEST or if DEST is `-', write to standard output.",
  "  If no SOURCE or if SOURCE is `-', read standard input.",
  progName ++ " --defaults",
  "  Dump a well documented set of default config values to standard output."
  ]

data Config = Config_
  { _excluded                            :: Maybe [String]
  , _instrumentMain                      :: Maybe Bool
  , _useHoedBackend                      :: Maybe Bool
  , _disablePartialTypeSignatureWarnings :: Maybe Bool
  , _enableExtendedDefaultingRules       :: Maybe Bool
  , _generateObservableInstances         :: Maybe Bool
  , _generateGenericInstances            :: Maybe Bool
  , _excludedFromInstanceGeneration      :: Maybe [String]
  , _verbose                             :: Maybe Bool
  } deriving (Generic, Show)

configJsonOptions :: Options
configJsonOptions = defaultOptions{fieldLabelModifier = tail}

instance FromJSON Config where parseJSON = genericParseJSON configJsonOptions
instance ToJSON Config where toJSON = genericToJSON configJsonOptions

{-# COMPLETE Config #-}
pattern Config { excluded
               , instrumentMain
               , useHoedBackend
               , disablePartialTypeSignatureWarnings
               , enableExtendedDefaultingRules
               , generateGenericInstances
               , generateObservableInstances
               , excludedFromInstanceGeneration
               , verbose
               } <-
  Config_
  { _excluded = (fromMaybe [] -> excluded)
  , _instrumentMain = (fromMaybe True -> instrumentMain)
  , _useHoedBackend = (fromMaybe False -> useHoedBackend)
  , _disablePartialTypeSignatureWarnings = (fromMaybe True -> disablePartialTypeSignatureWarnings)
  , _enableExtendedDefaultingRules = (fromMaybe True -> enableExtendedDefaultingRules)
  , _generateObservableInstances = (fromMaybe True -> generateObservableInstances)
  , _generateGenericInstances = (fromMaybe False -> generateGenericInstances)
  , _excludedFromInstanceGeneration = (fromMaybe [] -> excludedFromInstanceGeneration)
  , _verbose = (fromMaybe False -> verbose)
  }
  where Config a b c d e f g h i = Config_ (Just a) (Just b) (Just c) (Just d) (Just e) (Just f) (Just g) (Just h) (Just i)

defaultConfig :: Config
defaultConfig = Config_ Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

readConfig :: IO Config
readConfig = do
  cwd <- getCurrentDirectory
  home <- getHomeDirectory
  system <- getXdgDirectory XdgConfig "debug-pp"
  from <- filterM doesFileExist $
        [d </> ".debug-pp.yaml" | d <- reverse (ancestors cwd)] ++
        [ system </> "config.yaml"
        , home </> ".debug-pp.yaml"]
  case from of
    [] -> return defaultConfig
    _  -> loadYamlSettings from [] ignoreEnv
  where
    ancestors = map joinPath . tail . inits . splitPath

defConfig :: String
defConfig = unlines
  ["# debug-pp configuration file"
  ,"# ==========================="
  ,""
  ,"# List of Haskell module names to exclude from instrumentation"
  ,"excluded: []"
  , ""
  , "# If true, use the Hoed backend for trace generation."
  , "useHoedBackend: false"
  , ""
  , "# If true then insert a call to debugRun in the main function."
  , "instrumentMain: true"
  , ""
  , "# When the Hoed backend is used, instruct the debug TH wrapper to insert Observable instances for types that derive Generic."
  , "generateObservableInstances: true"
  , ""
  , "# When the Hoed backend is used, instruct the debug TH wrapper to insert Generic instances for types that don't derive Generic."
  , "generateGenericInstances: false"
  , ""
  , "# If the hoed backend is used, insert the ExtendedDefaultRules pragma."
  , "enableExtendedDefaultingRules: true"
  , ""
  , ""
  , "# List of types excluded from instance generation"
  , "excludedFromInstanceGeneration: []"
  , ""
  , "# If true, print a line for every instrumented module."
  , "verbose: false"
  , ""
  ]

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  (orig, inp, out) <- case args of
    ["--defaults"] -> do
      putStrLn defConfig
      exitSuccess
    ["--help"] -> do
      putStrLn $ usage progName
      exitSuccess
    []     -> return ("input",Nothing,Nothing)
    [i]    -> return (i, Just i, Nothing)
    [i,o]  -> return (i, Just i, Just o)
    [orig,i,o] -> return (orig, Just i, Just o)
    _ -> do
      putStrLn $ usage progName
      error "Unrecognized set of command line arguments"
  hIn  <- maybe (return stdin)  (`openFile` ReadMode) inp
  hOut <- maybe (return stdout) (`openFile` WriteMode) out
  contents <- hGetContents hIn
  config@Config{..} <- readConfig
  hPutStr hOut $ instrument config contents
  unless (hOut == stdout) $ hClose hOut
  when verbose $
    putStrLn $ "[debug-pp] Instrumented " ++ orig

instrument :: Config -> String -> String
instrument Config {..} contents
  | name `elem` excluded = contents
  | otherwise = unlines [top', modules', body'']
  where
    (top, name, modules, body) = parseModule contents
    debugModule = "Debug" ++ if useHoedBackend then ".Hoed" else ""
    modules' = unlines $ modules ++
      ["import qualified \"debug\"" ++ debugModule ++ " as Debug"] ++
      ["import qualified GHC.Generics" | generateGenericInstances]
    top' =
      unlines $
      [ "{-# LANGUAGE PackageImports #-}"
      , "{-# LANGUAGE TemplateHaskell #-}"
      , "{-# LANGUAGE PartialTypeSignatures #-}"
      , "{-# LANGUAGE ViewPatterns #-}"
      , "{-# LANGUAGE FlexibleContexts #-}"
      ] ++
      [ "{-# OPTIONS -Wno-partial-type-signatures #-}"
      | disablePartialTypeSignatureWarnings
      ] ++
      ["{-# LANGUAGE ExtendedDefaultRules #-}" | useHoedBackend && enableExtendedDefaultingRules] ++
      ["{-# LANGUAGE DeriveAnyClass #-}"       | useHoedBackend && (generateObservableInstances || generateGenericInstances)] ++
      ["{-# LANGUAGE DerivingStrategies #-}"   | useHoedBackend && (generateObservableInstances || generateGenericInstances)] ++
      ["{-# LANGUAGE DeriveGeneric #-}"        | useHoedBackend && generateGenericInstances] ++
      top
    body' =
      map
        (if instrumentMain
           then instrumentMainFunction
           else id)
        body
    debugWrapper
      | useHoedBackend && (generateGenericInstances || generateObservableInstances) =
        printf
          "Debug.debug' Debug.Config{Debug.generateGenericInstances=%s,Debug.generateObservableInstances=%s, Debug.excludeFromInstanceGeneration=%s}"
          (show generateGenericInstances)
          (show generateObservableInstances)
          (show excludedFromInstanceGeneration)
      | otherwise =
        "Debug.debug"
    body'' = unlines $ (debugWrapper ++ " [d|") : map indent (body' ++ ["  |]"])

instrumentMainFunction :: String -> String
instrumentMainFunction l
  | ('m':'a':'i':'n':rest) <- l
  , ('=':rest') <- dropWhile isSpace rest
  , not ("debugRun" `isPrefixOf` rest') = "main = Debug.debugRun $ " ++ rest'
  | otherwise = l

parseModule :: String -> ([String], String, [String], [String])
parseModule contents = (map fst top, name, modules, body)
  where
    contents' = annotateBlockComments (lines contents)
    moduleLine =
      findIndex (\(l,insideComment) -> not insideComment && isModuleLine l) contents'
    firstImportLine =
      findIndex (\(l, insideComment) -> not insideComment && isImportLine l) contents'
    lastPragmaLine =
      case takeWhile (\(_,(l, insideComment)) -> not insideComment && isPragmaLine l) (zip [(0::Int)..] contents') of
        [] -> Nothing
        xx -> Just $ fst $ last xx
    (top, rest)
      | Just l <- firstImportLine = splitAt (l-1) contents'
      | Just m <- moduleLine      = splitAt (m+1) contents'
      | Just p <- lastPragmaLine  = splitAt (p+1) contents'
      | otherwise = ([], contents')
    (reverse -> body0, reverse -> modules0) =
      break (\(l,insideComment) -> not insideComment && isImportLine l) (reverse rest)
    name
      | Just l <- moduleLine
      = takeWhile (\x -> not (isSpace x || x == '(')) $ drop 7 (fst $ contents' !! l)
      | otherwise
      = "Main"
    body = map fst $ dropWhile snd body0
    modules = map fst modules0 ++ map fst (takeWhile snd body0)

isModuleLine :: String -> Bool
isModuleLine l = "module " `isPrefixOf` l && all (\c -> isAlpha c || c `elem` " ().") l
isImportLine :: String -> Bool
isImportLine = ("import " `isPrefixOf`)
isPragmaLine :: String -> Bool
isPragmaLine = ("{-#" `isPrefixOf`)

indent :: String -> String
indent it@('#':_) = it
indent other      = "  " ++ other

-- Annotate every line with True if its inside the span of a block comment.
-- @
--   {- LANGUAGE foo -}     -- False
--   This is not inside {-  -- False
--   but this is -}         -- True
annotateBlockComments :: [String] -> [(String, Bool)]
annotateBlockComments = annotateBlockComments' False
annotateBlockComments' :: Bool -> [String] -> [(String, Bool)]
annotateBlockComments' _ [] = []
annotateBlockComments' False (l:rest) = (l,False) : annotateBlockComments' (startsBlockComment l) rest
annotateBlockComments' True  (l:rest) = (l,True) : annotateBlockComments' (not $ endsBlockComment l) rest

startsBlockComment :: String -> Bool
startsBlockComment line
    | Just l' <- dropUntilIncluding "{-" line = not $ endsBlockComment l'
    | otherwise = False

endsBlockComment :: String -> Bool
endsBlockComment line
    | Just l' <- dropUntilIncluding "-}" line = not $ startsBlockComment l'
    | otherwise = False

dropUntilIncluding :: Eq a => [a] -> [a] -> Maybe [a]
dropUntilIncluding needle haystack
  | [] <- haystack = Nothing
  | Just x <- stripPrefix needle haystack = Just x
  | x:rest <- haystack = (x:) <$> dropUntilIncluding needle rest
