
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}          -- IsoFunctorStageOne (MumuIso Maybe x)
{-# LANGUAGE MultiParamTypeClasses #-}      -- IsoFunctorStageTwo
{-# LANGUAGE TypeFamilies #-}


{-| Non-polymorphic semi-isomorphisms.
-}

module Sky.Isomorphism.MumuIsoFunctor where

import Prelude hiding (id, (.))
import Control.Category                     -- yay

import Data.Tuple (swap)
import Control.Monad ((<=<))

-- Note: Isomorphisms should be instances of Category.

----------------------------------------------------------------------------------------------------

class (Category i, Monad (MonadT i)) => SemiIsomorphism i where
    type MonadT i :: * -> *
    --type MonadT i = m

    packSemiIsomorphism :: (a -> (MonadT i) b, b -> (MonadT i) a) -> i a b
    unpackSemiIsomorphism :: i a b -> (a -> (MonadT i) b, b -> (MonadT i) a)
    -- Derived
    toSemiIsomorphism :: (a -> (MonadT i) b) -> (b -> (MonadT i) a) -> i a b
    toSemiIsomorphism amb bma = packSemiIsomorphism (amb, bma)
    applySemiIsomorphism :: i a b -> a -> (MonadT i) b
    applySemiIsomorphism = fst . unpackSemiIsomorphism
    unapplySemiIsomorphism :: i a b -> b -> (MonadT i) a
    unapplySemiIsomorphism = snd . unpackSemiIsomorphism
    revertSemiIsomorphism :: i a b -> i b a
    revertSemiIsomorphism = packSemiIsomorphism . swap . unpackSemiIsomorphism
    convertSemiIsomorphism :: (SemiIsomorphism j, MonadT j ~ MonadT i) => j a b -> i a b
    convertSemiIsomorphism = packSemiIsomorphism . unpackSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Operators and stuff

iso :: forall i a b. (SemiIsomorphism i) => (a -> (MonadT i) b) -> (b -> (MonadT i) a) -> i a b
iso = toSemiIsomorphism

apply :: forall i a b. (SemiIsomorphism i) => i a b -> a -> (MonadT i) b
apply = applySemiIsomorphism

unapply :: forall i a b. (SemiIsomorphism i) => i a b -> b -> (MonadT i) a
unapply = unapplySemiIsomorphism

convert :: forall i j a b. (SemiIsomorphism i, SemiIsomorphism j, MonadT i ~ MonadT j) => i a b -> j a b
convert = convertSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Simple semi-isomorphism

newtype MumuIso m a b = MumuIso { _rawMumuIso :: (a -> m b, b -> m a) }

instance Monad m => Category (MumuIso m) where
    id = MumuIso (return, return)
    (MumuIso (applyF, unapplyF)) . (MumuIso (applyG, unapplyG)) =
        MumuIso ((applyF <=< applyG), (unapplyG <=< unapplyF))

instance (Monad m) => SemiIsomorphism (MumuIso m) where
    type MonadT (MumuIso m) = m
    packSemiIsomorphism = MumuIso
    unpackSemiIsomorphism = _rawMumuIso

----------------------------------------------------------------------------------------------------
-- Semi-Isomorphisms can be used as IsoFunctors!

class IsoFunctorStageOne f where
    isomap1 :: forall a b. MumuIso Maybe a b -> f a -> f b

instance IsoFunctorStageOne (MumuIso Maybe x) where
    isomap1 :: forall a b. MumuIso Maybe a b -> MumuIso Maybe x a -> MumuIso Maybe x b
    isomap1 ab xa = ab . xa

----------------------------------------------------------------------------------------------------
-- But we'd like to make it work with any monad m

class (Monad (MonadStageTwo f)) => IsoFunctorStageTwo f where
    type MonadStageTwo f :: * -> *
    isomap2 :: forall a b. MumuIso (MonadStageTwo f) a b -> f a -> f b

instance (Monad m) => IsoFunctorStageTwo (MumuIso m x) where
    type MonadStageTwo (MumuIso m x) = m

    isomap2 :: forall a b. MumuIso m a b -> MumuIso m x a -> MumuIso m x b
    isomap2 ab xa = ab . xa

----------------------------------------------------------------------------------------------------
-- Now make it work for _ANY SEMIISOMORPHISM_

class (Monad (MonadStageThree f)) => IsoFunctorStageThree f where
    type MonadStageThree f :: * -> *
    isomap3 :: forall a b i. (SemiIsomorphism i, MonadT i ~ MonadStageThree f) => i a b -> f a -> f b

instance (Monad m) => IsoFunctorStageThree (MumuIso m x) where
    type MonadStageThree (MumuIso m x) = m

    isomap3 :: forall a b i. (SemiIsomorphism i, MonadT i ~ m) => i a b -> MumuIso m x a -> MumuIso m x b
    isomap3 ab xa = (convert ab) . xa

----------------------------------------------------------------------------------------------------
-- Can we make it work with _ANY 2 SEMIISOMORPHISMS_ ?

-- Same as IsoFunctorStageThree, redefined to avoid conflicts
class IsoFunctorStageFour f where
    type MonadStageFour f :: * -> *
    isomap4 :: forall a b i. (SemiIsomorphism i, MonadT i ~ MonadStageFour f) => i a b -> f a -> f b

instance (SemiIsomorphism j) => IsoFunctorStageFour (j x) where
    type MonadStageFour (j x) = MonadT j

    isomap4 :: forall a b i. (SemiIsomorphism i, MonadT i ~ MonadT j) => i a b -> j x a -> j x b
    isomap4 ab xa = (convert ab) . xa
