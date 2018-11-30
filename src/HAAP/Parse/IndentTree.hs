module HAAP.Parse.IndentTree where

import Data.Tree

import Control.Applicative
import Data.Char (isSpace)
import Data.Monoid
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Indent

parseIndentedForest :: String -> Either ParseError (Forest String)
parseIndentedForest = runIndentParser aForest () "" 
    where
    aForest = many aTree
    aTree = spaces *> withBlock Node aNodeHeader aTree
    aNodeHeader = many1 (satisfy (/='\n')) <* spaces

prettyForest i = unlines . map (prettyTree i)
prettyTree :: Int -> Tree String -> String
prettyTree i (Node n ts) = replicate i ' ' ++ n ++ "\n" ++ prettyForest (succ i) ts