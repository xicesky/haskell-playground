
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}

{-| Non-polymorphic semi-isomorphisms.
-}

module Old.Sky.Isomorphism.Simple
    ( SemiIsomorphism
    , packSemiIsomorphism
    , unpackSemiIsomorphism
    , toSemiIsomorphism
    , applySemiIsomorphism
    , unapplySemiIsomorphism
    , revertSemiIsomorphism
    , convertSemiIsomorphism
    , iso
    , apply
    , unapply
    , convert
    , MumuIso
    ) where

import Prelude hiding (id, (.))
import Control.Category                     -- yay

import Data.Tuple (swap)
import Control.Monad ((<=<))

-- Note: Isomorphisms should be instances of Category.

----------------------------------------------------------------------------------------------------

class SemiIsomorphism i where
    packSemiIsomorphism :: Monad m => (a -> m b, b -> m a) -> i m a b
    unpackSemiIsomorphism :: Monad m => i m a b -> (a -> m b, b -> m a)
    -- Derived
    toSemiIsomorphism :: Monad m => (a -> m b) -> (b -> m a) -> i m a b
    toSemiIsomorphism amb bma = packSemiIsomorphism (amb, bma)
    applySemiIsomorphism :: Monad m => i m a b -> a -> m b
    applySemiIsomorphism = fst . unpackSemiIsomorphism
    unapplySemiIsomorphism :: Monad m => i m a b -> b -> m a
    unapplySemiIsomorphism = snd . unpackSemiIsomorphism
    revertSemiIsomorphism :: Monad m => i m a b -> i m b a
    revertSemiIsomorphism = packSemiIsomorphism . swap . unpackSemiIsomorphism
    convertSemiIsomorphism :: (Monad m, SemiIsomorphism j) => j m a b -> i m a b
    convertSemiIsomorphism = packSemiIsomorphism . unpackSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Operators and stuff

iso :: forall i m a b. (SemiIsomorphism i, Monad m) => (a -> m b) -> (b -> m a) -> i m a b
iso = toSemiIsomorphism

apply :: forall i m a b. (SemiIsomorphism i, Monad m) => i m a b -> a -> m b
apply = applySemiIsomorphism

unapply :: forall i m a b. (SemiIsomorphism i, Monad m) => i m a b -> b -> m a
unapply = unapplySemiIsomorphism

convert :: forall i j m a b. (SemiIsomorphism i, Monad m, SemiIsomorphism j) => i m a b -> j m a b
convert = convertSemiIsomorphism

----------------------------------------------------------------------------------------------------
-- Simple semi-isomorphism

newtype MumuIso m a b = MumuIso { _rawMumuIso :: (a -> m b, b -> m a) }

instance Monad m => Category (MumuIso m) where
    id = MumuIso (return, return)
    (MumuIso (applyF, unapplyF)) . (MumuIso (applyG, unapplyG)) =
        MumuIso ((applyF <=< applyG), (unapplyG <=< unapplyF))

instance SemiIsomorphism MumuIso where
    packSemiIsomorphism = MumuIso
    unpackSemiIsomorphism = _rawMumuIso

----------------------------------------------------------------------------------------------------
-- Ignore this!

class IsoFunctor f where
    isomap :: forall i m a b x. (SemiIsomorphism i, Monad m) => i m a b -> f m x a -> f m x b

instance IsoFunctor MumuIso where
    isomap :: forall i m a b x. (SemiIsomorphism i, Monad m) => i m a b -> MumuIso m x a -> MumuIso m x b
    isomap isoA isoB = (convert isoA) . isoB where
