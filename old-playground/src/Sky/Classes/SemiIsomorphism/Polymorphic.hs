
{-# LANGUAGE ScopedTypeVariables #-}

module Sky.Classes.SemiIsomorphism.Polymorphic where

import Data.Tuple (swap)

class PolymorphicSemiIsomorphism i where
    packPolymorphicSemiIsomorphism :: Monad m => (s -> m a, a' -> m s') -> i m s s' a a'
    unpackPolymorphicSemiIsomorphism :: Monad m => i m s s' a a' -> (s -> m a, a' -> m s')
    -- Derived
    toPolymorphicSemiIsomorphism :: Monad m => (s -> m a) -> (a' -> m s') -> i m s s' a a'
    toPolymorphicSemiIsomorphism sma a'ms' = packPolymorphicSemiIsomorphism (sma, a'ms')
    applyPolymorphicSemiIsomorphism :: Monad m => i m s s' a a' -> s -> m a
    applyPolymorphicSemiIsomorphism = fst . unpackPolymorphicSemiIsomorphism
    unapplyPolymorphicSemiIsomorphism :: Monad m => i m s s' a a' -> a' -> m s'
    unapplyPolymorphicSemiIsomorphism = snd . unpackPolymorphicSemiIsomorphism
    reversePolymorphicSemiIsomorphism :: Monad m => i m s s' a a' -> i m a' a s' s
    reversePolymorphicSemiIsomorphism = packPolymorphicSemiIsomorphism . swap . unpackPolymorphicSemiIsomorphism
    convertPolymorphicSemiIsomorphism :: (Monad m, PolymorphicSemiIsomorphism j) => j m s s' a a' -> i m s s' a a'
    convertPolymorphicSemiIsomorphism = packPolymorphicSemiIsomorphism . unpackPolymorphicSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Operators and stuff

iso :: forall i m s s' a a'. (PolymorphicSemiIsomorphism i, Monad m) => (s -> m a) -> (a' -> m s') -> i m s s' a a'
iso = toPolymorphicSemiIsomorphism

apply :: forall i m s s' a a'. (PolymorphicSemiIsomorphism i, Monad m) => i m s s' a a' -> s -> m a
apply = applyPolymorphicSemiIsomorphism

unapply :: forall i m s s' a a'. (PolymorphicSemiIsomorphism i, Monad m) => i m s s' a a' -> a' -> m s'
unapply = unapplyPolymorphicSemiIsomorphism

convert :: forall i j m s s' a a'. (PolymorphicSemiIsomorphism i, Monad m, PolymorphicSemiIsomorphism j) => i m s s' a a' -> j m s s' a a'
convert = convertPolymorphicSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Polymorphic isomorphisms can't be made an actual instance of category
-- although they _do form a category_

-- TODO: Provide a class "Category2" ?
