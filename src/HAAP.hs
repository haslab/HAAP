{-
HAAP: Haskell Automated Assessment Platform

This module provides a convient interface that imports all the HAAP features.
-}

module HAAP
    ( module HAAP.Core
    , module HAAP.DB
    , module HAAP.DB.State
    , module HAAP.DB.Acid
    , module HAAP.DB.Binary
    , module HAAP.IO
    , module HAAP.Lens
    , module HAAP.Log
    , module HAAP.Sources
    , module HAAP.Sources.SVN
    , module HAAP.Sources.FilePath
    , module HAAP.Utils
    , module HAAP.Test.Spec
    , module HAAP.Test.Rank
    , module HAAP.Test.Tourney
    , module HAAP.Pretty
    , module HAAP.Web.Hakyll
    , module HAAP.Web.Blaze
    , module HAAP.Web.Test.Spec
    , module HAAP.Web.Test.Rank
    , module HAAP.Web.Test.Tourney
    , module HAAP.Doc.Haddock
    , module HAAP.Code.HLint
    , module HAAP.Code.Homplexity
    , module HAAP.Code.HPC
    , module HAAP.Compiler.GHC
    , module HAAP.Compiler.GHCJS
    , module HAAP.Web.Graphics.CodeWorld
    , module HAAP.Web.HTML.Table
    , module HAAP.Template
    , module HAAP.Web.PHP.Login
    , module HAAP.Web.HTML.Pandoc
    , module HAAP.Web.HTML.TagSoup
    , module HAAP.Code.Haskell
    , module HAAP.Code.Analysis.Haddock
    , module HAAP.Code.Git
    , module HAAP.Code.Analysis.Modularity
    , module HAAP.Code.Analysis.SourceGraph
    , module HAAP.Code.Analysis.Usage
    , module HAAP.Shelly
    , module HAAP.Plugin
    , module HAAP.Graphics.Gloss.Capture
    , module HAAP.CmdArgs
    , module HAAP.Web.Graphics.Xterm
    ) where

import HAAP.Core
import HAAP.CmdArgs
import HAAP.Code.Analysis.Haddock
import HAAP.Code.Haskell
import HAAP.DB
import HAAP.Shelly
import HAAP.DB.State
import HAAP.DB.Acid
import HAAP.DB.Binary
import HAAP.IO
import HAAP.Lens hiding (Context(..))
import HAAP.Log
import HAAP.Sources
import HAAP.Sources.SVN
import HAAP.Sources.FilePath
import HAAP.Utils
import HAAP.Test.Spec
import HAAP.Test.Rank
import HAAP.Test.Tourney
import HAAP.Pretty
import HAAP.Web.Hakyll hiding (applyTemplate)
import HAAP.Web.Blaze
import HAAP.Web.Test.Spec
import HAAP.Web.Test.Rank
import HAAP.Web.Test.Tourney
import HAAP.Doc.Haddock
import HAAP.Code.HLint
import HAAP.Code.Homplexity
import HAAP.Code.HPC
import HAAP.Compiler.GHC
import HAAP.Compiler.GHCJS
import HAAP.Web.Graphics.CodeWorld
import HAAP.Web.HTML.Table
import HAAP.Template
import HAAP.Web.PHP.Login
import HAAP.Web.HTML.Pandoc
import HAAP.Web.HTML.TagSoup
import HAAP.Code.Git
import HAAP.Code.Analysis.Modularity
import HAAP.Code.Analysis.SourceGraph
import HAAP.Code.Analysis.Usage
import HAAP.Plugin
import HAAP.Graphics.Gloss.Capture
import HAAP.Web.Graphics.Xterm



