{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-
  Copyright 2017 The CodeWorld Authors. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

--------------------------------------------------------------------------------
-- |The standard set of functions and variables available to all programs.
--
-- You may use any of these functions and variables without defining them.
module Prelude
    ( module Prelude
    , error, undefined, fail, trace, putStrLn, putStr, print
    ) where

import "base" Prelude hiding (error,undefined,fail,putStrLn,putStr,print)
import qualified HaskellPrelude as P

import CodeWorld as CW hiding (trace)
import qualified CodeWorld as CW
import System.IO.Unsafe

import Data.JSString.Text
import Data.String

import GHC.Stack (HasCallStack, withFrozenCallStack)

-- | Fails with an error message.
error :: HasCallStack => String -> a
error msg = trace msg $ withFrozenCallStack (P.error msg)

-- | Represents an undefined value.  This lets you compile programs with unfinished
-- values.  If the value is needed, the program will crash.
undefined :: HasCallStack => a
undefined = trace msg $ withFrozenCallStack (P.error msg)
    where msg = "Value is not defined."

-- | Fails with an error message.  This is required (though apparently unused)
-- by the desugaring for pattern binds in list comprehensions.
fail :: (Monad m,HasCallStack) => String -> m a
fail msg = trace msg $ withFrozenCallStack (P.fail msg)

trace :: String -> a -> a
trace msg x = CW.trace (fromString msg) x

putStrLn :: String -> IO ()
putStrLn msg = trace (msg++"\n") $ return ()

putStr :: String -> IO ()
putStr msg = trace msg $ return ()

print :: Show a => a -> IO ()
print x = trace (show x) $ return ()

