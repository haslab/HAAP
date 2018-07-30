{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Auth.Exts where

import Snap
import Snap.Snaplet
import Snap.Snaplet.Auth

import Data.ByteString
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe

loginUser' :: ByteString
           -> ByteString
           -> Maybe ByteString
           -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
loginUser' unf pwdf remf = do
    mbUsername <- getParam unf
    mbPassword <- getParam pwdf
    remember   <- liftM (fromMaybe False)
                    (runMaybeT $
                    do field <- MaybeT $ return remf
                       value <- MaybeT $ getParam field
                       return $ value == "1" || value == "on")

    case mbUsername of
      Nothing -> return $ Left UsernameMissing
      Just u -> case mbPassword of
        Nothing -> return $ Left PasswordMissing
        Just p -> loginByUsername (decodeUtf8 u) (ClearText p) remember