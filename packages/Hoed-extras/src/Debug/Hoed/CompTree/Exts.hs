{-# LANGUAGE DeriveGeneric, StandaloneDeriving, PackageImports, FlexibleInstances #-}

module Debug.Hoed.CompTree.Exts (
    Graph(..), Judgement(..),AssistedMessage(..),Arc(..),H.Vertex(..),H.CompStmt(..),H.StmtDetails(..),Hashed(..)
    ,showCompTree,readCompTree,prettyCompStmt,prettyVertex
    ) where

import Data.Graph.Libgraph
import Data.Hashable
import qualified "Hoed" Debug.Hoed as H
import qualified Debug.Hoed.Render as H
import Debug.Hoed.Utils

import GHC.Generics
import Data.Text (Text(..))
import Control.Monad
import Text.Read
import qualified Data.Text as T
import Data.Binary (Binary(..))
import qualified Data.Binary as Binary

instance (Binary a,Binary b) => Binary (Graph a b)
instance Binary Judgement
instance Binary AssistedMessage
instance (Binary a,Binary b) => Binary (Arc a b)
instance Binary H.Vertex
instance Binary H.CompStmt
instance Binary H.StmtDetails
instance (Hashable a,Binary a) => Binary (Hashed a) where
    get = liftM hashed Binary.get
    put = Binary.put . unhashed

showCompTree :: H.CompTree -> String
showCompTree = show . mapGraph chgVertex

readCompTree :: String -> H.CompTree
readCompTree = read

chgVertex :: H.Vertex -> Vertex
chgVertex H.RootVertex = RootVertex
chgVertex (H.Vertex s j) = Vertex (chgCompStmt s) j

chgCompStmt :: H.CompStmt -> CompStmt
chgCompStmt (H.CompStmt l i d) = CompStmt l i d

data CompStmt = CompStmt { stmtLabel      :: !Text
                         , stmtIdentifier :: !Int
                         , stmtDetails    :: !H.StmtDetails
                         }
                deriving (Read,Show,Generic)

data Vertex = RootVertex | Vertex {vertexStmt :: CompStmt, vertexJmt :: Judgement}
  deriving (Read,Show,Generic)

deriving instance Show H.StmtDetails
deriving instance (Show a,Show b) => Show (Graph a b)
deriving instance (Read a,Read b) => Read (Graph a b)
deriving instance Read H.StmtDetails
deriving instance Read Judgement
deriving instance Read AssistedMessage

instance {-# OVERLAPS #-} Show (Hashed Text) where
    showsPrec i = showsPrec i . unhashed
instance {-# OVERLAPS #-} Read (Hashed Text) where
    readPrec = hashed <$> readPrec
deriving instance (Read a,Read b) => Read (Arc a b)
deriving instance Read H.Vertex
deriving instance Read H.CompStmt

prettyCompStmt :: H.CompStmt -> String
prettyCompStmt (H.CompStmt l i (H.StmtCon c _)) = T.unpack (trimText l) ++ " = " ++ T.unpack (unhashed c)
prettyCompStmt (H.CompStmt l i (H.StmtLam args res _)) = sepByStr " " (T.unpack (trimText l):map (T.unpack . unhashed) args) ++ " = " ++ T.unpack (unhashed res)

sepByStr :: String -> [String] -> String
sepByStr s [] = []
sepByStr s [x] = x
sepByStr s (x:xs) = x ++ s ++ sepByStr s xs

prettyVertex :: H.Vertex -> String
prettyVertex H.RootVertex = "root"
prettyVertex (H.Vertex s j) = prettyCompStmt s




