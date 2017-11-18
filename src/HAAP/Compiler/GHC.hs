module HAAP.Compiler.GHC where
    
import HAAP.IO

import Data.Default

import Shelly (Sh(..))
import qualified Shelly as Sh

data GHCArgs = GHCArgs
    { ghcSafe :: Bool -- compile with the -XSafe extension
    , ghcHpc :: Bool -- compile for hpc
    , ghcArgs :: [String] -- additional flags
    , ghcRTS :: Bool
    }

instance Default GHCArgs where
    def = GHCArgs True False [] False

shGhcWith :: IOArgs -> GHCArgs -> [FilePath] -> Sh IOResult
shGhcWith io ghc ins = shCommandWith io "ghc" $ addArgs (ghcArgs ghc) $ addRTS (ghcRTS ghc) $ addHpc (ghcHpc ghc) $ addSafe (ghcSafe ghc) ins
    where
    addSafe True cmds = "-XSafe" : cmds
    addSafe False cmds = cmds
    addHpc True  cmds = "-fhpc" : cmds
    addHpc False cmds = cmds
    addRTS True  cmds = "-rtsopts" : cmds
    addRTS False cmds = cmds
    addArgs xs ys = ys ++ xs