
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Old.Sky.Classes.Isomorphism.Polymorphic where

import Old.Sky.Classes.SemiIsomorphism.Polymorphic
import Data.Tuple (swap)
import Data.Functor.Identity

class PolymorphicIsomorphism i where
    packPolymorphicIsomorphism :: (s -> a, a' -> s') -> i s s' a a'
    unpackPolymorphicIsomorphism :: i s s' a a' -> (s -> a, a' -> s')
    -- Derived
    toPolymorphicIsomorphism :: (s -> a) -> (a' -> s') -> i s s' a a'
    toPolymorphicIsomorphism sma bmt = packPolymorphicIsomorphism (sma, bmt)
    applyPolymorphicIsomorphism :: i s s' a a' -> s -> a
    applyPolymorphicIsomorphism = fst . unpackPolymorphicIsomorphism
    unapplyPolymorphicIsomorphism :: i s s' a a' -> a' -> s'
    unapplyPolymorphicIsomorphism = snd . unpackPolymorphicIsomorphism
    revertPolymorphicIsomorphism :: i s s' a a' -> i a' a s' s
    revertPolymorphicIsomorphism = packPolymorphicIsomorphism . swap . unpackPolymorphicIsomorphism
    convertPolymorphicIsomorphism :: (PolymorphicIsomorphism j) => j s s' a a' -> i s s' a a'
    convertPolymorphicIsomorphism = packPolymorphicIsomorphism . unpackPolymorphicIsomorphism

----------------------------------------------------------------------------------------------------
-- Operators and stuff

iso :: forall i s s' a a'. (PolymorphicIsomorphism i) => (s -> a) -> (a' -> s') -> i s s' a a'
iso = toPolymorphicIsomorphism

apply :: forall i s s' a a'. (PolymorphicIsomorphism i) => i s s' a a' -> s -> a
apply = applyPolymorphicIsomorphism

unapply :: forall i s s' a a'. (PolymorphicIsomorphism i) => i s s' a a' -> a' -> s'
unapply = unapplyPolymorphicIsomorphism

convert :: forall i j s s' a a'. (PolymorphicIsomorphism i, PolymorphicIsomorphism j) => i s s' a a' -> j s s' a a'
convert = convertPolymorphicIsomorphism

----------------------------------------------------------------------------------------------------
-- Any polymorphic Semi-Isomorphism is just an isomorphism when we use the identity monad

instance (PolymorphicSemiIsomorphism i) => PolymorphicIsomorphism (i Identity) where
    packPolymorphicIsomorphism :: forall s s' a a'. (s -> a, a' -> s') -> i Identity s s' a a'
    packPolymorphicIsomorphism (sa, as) = packPolymorphicSemiIsomorphism (sma, ams) where
        sma :: s -> Identity a
        sma = return . sa
        ams :: a' -> Identity s'
        ams = return . as

    unpackPolymorphicIsomorphism :: forall s s' a a'. i Identity s s' a a' -> (s -> a, a' -> s')
    unpackPolymorphicIsomorphism iso = (sa, as) where
        (sma, ams) = unpackPolymorphicSemiIsomorphism iso
        sa :: s -> a
        sa = runIdentity . sma
        as :: a' -> s'
        as = runIdentity . ams
