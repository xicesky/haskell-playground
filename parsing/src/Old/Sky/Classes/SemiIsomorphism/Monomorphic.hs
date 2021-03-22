
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Old.Sky.Classes.SemiIsomorphism.Monomorphic where

import Data.Tuple (swap)
import Control.Monad ((<=<))
import Control.Category (Category)
import qualified Control.Category as Cat

class MonomorphicSemiIsomorphism i where
    packMonomorphicSemiIsomorphism :: Monad m => (s -> m a, a -> m s) -> i m s a
    unpackMonomorphicSemiIsomorphism :: Monad m => i m s a -> (s -> m a, a -> m s)
    -- Derived
    toMonomorphicSemiIsomorphism :: Monad m => (s -> m a) -> (a -> m s) -> i m s a
    toMonomorphicSemiIsomorphism sma ams' = packMonomorphicSemiIsomorphism (sma, ams')
    applyMonomorphicSemiIsomorphism :: Monad m => i m s a -> s -> m a
    applyMonomorphicSemiIsomorphism = fst . unpackMonomorphicSemiIsomorphism
    unapplyMonomorphicSemiIsomorphism :: Monad m => i m s a -> a -> m s
    unapplyMonomorphicSemiIsomorphism = snd . unpackMonomorphicSemiIsomorphism
    reverseMonomorphicSemiIsomorphism :: Monad m => i m s a -> i m a s
    reverseMonomorphicSemiIsomorphism = packMonomorphicSemiIsomorphism . swap . unpackMonomorphicSemiIsomorphism
    convertMonomorphicSemiIsomorphism :: (Monad m, MonomorphicSemiIsomorphism j) => j m s a -> i m s a
    convertMonomorphicSemiIsomorphism = packMonomorphicSemiIsomorphism . unpackMonomorphicSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Operators and stuff

iso :: forall i m s a. (MonomorphicSemiIsomorphism i, Monad m) => (s -> m a) -> (a -> m s) -> i m s a
iso = toMonomorphicSemiIsomorphism

apply :: forall i m s a. (MonomorphicSemiIsomorphism i, Monad m) => i m s a -> s -> m a
apply = applyMonomorphicSemiIsomorphism

unapply :: forall i m s a. (MonomorphicSemiIsomorphism i, Monad m) => i m s a -> a -> m s
unapply = unapplyMonomorphicSemiIsomorphism

convert :: forall i j m s a. (MonomorphicSemiIsomorphism i, Monad m, MonomorphicSemiIsomorphism j) => i m s a -> j m s a
convert = convertMonomorphicSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Monomorphic isomorphisms can be made an actual instance of category

instance (Monad m, MonomorphicSemiIsomorphism i) => Category (i m) where
    id :: i m a a
    id = packMonomorphicSemiIsomorphism (return, return)

    (.) :: i m b c -> i m a b -> i m a c
    (.) isoBC isoAB = isoAC where
        (bmc, cmb) = unpackMonomorphicSemiIsomorphism isoBC
        (amb, bma) = unpackMonomorphicSemiIsomorphism isoAB
        amc = bmc <=< amb
        cma = bma <=< cmb
        isoAC = packMonomorphicSemiIsomorphism (amc, cma)
