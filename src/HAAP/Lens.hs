{-# LANGUAGE RankNTypes #-}

module HAAP.Lens
    ( module Control.Lens
    , module HAAP.Lens
    ) where

import HAAP.Core

import Control.Monad
import Control.Lens hiding (snoc)
import Control.Monad.State (MonadState(..))
import qualified Control.Monad.State as State

newtype Lens'' s v = Lens'' { unLens'' :: Lens' s v }

viewLens'' :: Lens'' s v -> (s -> v)
viewLens'' (Lens'' l) = view l

setLens'' :: Lens'' s v -> (v -> s -> s)
setLens'' (Lens'' l) = set l

idLens'' :: Lens'' a a
idLens'' = Lens'' $ lens id (curry snd)

constLens'' :: a -> Lens'' () a
constLens'' x = Lens'' $ lens (const x) (\_ _ -> ())

getStateLens'' :: MonadState st m => Lens'' st s -> m s
getStateLens'' l = liftM (^. unLens'' l) State.get

haapDBLens'' :: HaapMonad m => Lens'' st1 st2 -> Haap p args st2 m a -> Haap p args st1 m a
haapDBLens'' l m = mapHaapDB get' put' m
    where
    get' = liftM (viewLens'' l) getDB
    put' st2 = do
        st1 <- getDB
        putDB $ setLens'' l st2 st1
