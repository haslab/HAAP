module HAAP.Compiler.GHCJS where
    
import HAAP.IO

import Data.Default

import Shelly (Sh(..))
import qualified Shelly as Sh

data GHCJSArgs = GHCJSArgs
    { ghcjsSafe :: Bool -- compile with the -XSafe extension
    , ghcjsArgs :: [String] -- additional flags
    }

instance Default GHCJSArgs where
    def = GHCJSArgs True []

shGhcjsWith :: IOArgs -> GHCJSArgs -> [FilePath] -> Sh IOResult
shGhcjsWith io ghc ins = shCommandWith io "ghcjs" $ addArgs (ghcjsArgs ghc) $ addSafe (ghcjsSafe ghc) ins
    where
    addSafe True cmds = "-XSafe" : cmds
    addSafe False cmds = cmds
    addArgs xs ys = ys ++ xs

ioGhcjsWith :: IOArgs -> GHCJSArgs -> [FilePath] -> IO IOResult
ioGhcjsWith io ghc ins = do
    ioCommandWith io "ghcjs" (addArgs (ghcjsArgs ghc) $ addSafe (ghcjsSafe ghc) ins)
  where
    addSafe True cmds = "-XSafe" : cmds
    addSafe False cmds = cmds
    addArgs xs ys = ys ++ xs