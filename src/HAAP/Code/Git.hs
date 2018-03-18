module HAAP.Code.Git where

import HAAP.IO
import HAAP.Core
import HAAP.Shelly

import qualified Data.Text as Text

import Shelly

shGitVersion :: Sh String
shGitVersion = do
    res <- shCommand "git" ["log","-n","1"]
    let out = Text.unpack $ resStdout res
    let v = last $ words $ head $ lines out
    return v