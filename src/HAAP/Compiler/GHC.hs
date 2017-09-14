module HAAP.Compiler.GHC where
    
import HAAP.IO

import Data.Default

import Shelly (Sh(..))
import qualified Shelly as Sh

data GHCArgs = GHCArgs
    { ghcSafe :: Bool -- compile with the -XSafe extension
    , ghcHpc :: Bool -- compile for hpc
    , ghcArgs :: [String] -- additional flags
    }

instance Default GHCArgs where
    def = GHCArgs True False []

shGhcWith :: IOArgs -> GHCArgs -> [FilePath] -> Sh IOResult
shGhcWith io ghc ins = shCommandWith io "ghc" $ addArgs (ghcArgs ghc) $ addHpc (ghcHpc ghc) $ addSafe (ghcSafe ghc) ins
    where
    addSafe True cmds = "-XSafe" : cmds
    addSafe False cmds = cmds
    addHpc True  cmds = "-fhpc" : cmds
    addHpc False cmds = cmds
    addArgs xs ys = ys ++ xs