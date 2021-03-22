
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}       -- Category (MumuIso' m)

module Old.Sky.Isomorphism.Class where

import Prelude hiding (id, (.))
import Control.Category                     -- yay

import Data.Tuple (swap)

-- Note: Isomorphisms should be instances of Category, but categories can't deal
--  with polymorphic type changes.

----------------------------------------------------------------------------------------------------

class SemiIsomorphism i where
    packSemiIsomorphism :: Monad m => (s -> m a, b -> m t) -> i m s t a b
    unpackSemiIsomorphism :: Monad m => i m s t a b -> (s -> m a, b -> m t)
    -- Derived
    toSemiIsomorphism :: Monad m => (s -> m a) -> (b -> m t) -> i m s t a b
    toSemiIsomorphism sma bmt = packSemiIsomorphism (sma, bmt)
    applySemiIsomorphism :: Monad m => i m s t a b -> s -> m a
    applySemiIsomorphism = fst . unpackSemiIsomorphism
    unapplySemiIsomorphism :: Monad m => i m s t a b -> b -> m t
    unapplySemiIsomorphism = snd . unpackSemiIsomorphism
    revertSemiIsomorphism :: Monad m => i m s t a b -> i m b a t s
    revertSemiIsomorphism = packSemiIsomorphism . swap . unpackSemiIsomorphism

----------------------------------------------------------------------------------------------------

data MumuIso m s t a b = MumuIso { _rawMumuIso :: (s -> m a, b -> m t) }

-- Can't make this work...
--type MumuIso' m s a = MumuIso m s s a a
--instance Monad m => Category (MumuIso' m) where
--    id = MumuIso (id, id)
--    (MumuIso (applyF, unapplyF)) . (MumuIso (applyG, unapplyG)) =
--        MumuIso ((applyF . applyG), (unapplyG . unapplyF))

instance SemiIsomorphism MumuIso where
    packSemiIsomorphism = MumuIso
    unpackSemiIsomorphism = _rawMumuIso

----------------------------------------------------------------------------------------------------

-- Operators and stuff
iso :: forall i m s t a b. (SemiIsomorphism i, Monad m) => (s -> m a) -> (b -> m t) -> i m s t a b
iso = toSemiIsomorphism

apply :: forall i m s t a b. (SemiIsomorphism i, Monad m) => i m s t a b -> s -> m a
apply = applySemiIsomorphism

unapply :: forall i m s t a b. (SemiIsomorphism i, Monad m) => i m s t a b -> b -> m t
unapply = unapplySemiIsomorphism
