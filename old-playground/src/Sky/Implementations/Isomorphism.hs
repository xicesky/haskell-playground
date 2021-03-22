
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TypeSynonymInstances #-}

module Sky.Implementations.Isomorphism where

import Sky.Classes.Isomorphism
import Sky.Classes.IsoFunctor

import Data.Tuple (swap)
import Data.Functor.Identity
import Control.Category (Category)
import qualified Control.Category as Cat

newtype PackedSemiIso m s s' a a' = PackedSemiIso { _rawPackedSemiIso :: (s -> m a, a' -> m s') }

type PackedSemiIso' m s a = PackedSemiIso m s s a a

instance PolymorphicSemiIsomorphism PackedSemiIso where
    packPolymorphicSemiIsomorphism :: Monad m => (s -> m a, a' -> m s') -> PackedSemiIso m s s' a a'
    packPolymorphicSemiIsomorphism = PackedSemiIso
    unpackPolymorphicSemiIsomorphism :: Monad m => PackedSemiIso m s s' a a' -> (s -> m a, a' -> m s')
    unpackPolymorphicSemiIsomorphism = _rawPackedSemiIso

{- This would be nice to have, but is not possible in Haskell -}
--instance MonomorphicSemiIsomorphism PackedSemiIso' where
--    packMonomorphicSemiIsomorphism :: Monad m => (s -> m a, a -> m s) -> i m s a
--    packMonomorphicSemiIsomorphism = PackedSemiIso
--    unpackMonomorphicSemiIsomorphism :: Monad m => i m s a -> (s -> m a, a -> m s)
--    unpackMonomorphicSemiIsomorphism = _rawPackedSemiIso

{-  An instance PolymorphicIsomorphism (PackedSemiIso Identity) is already supplied in
    the module Sky.Isomorphism.Polymorphic, via:
        instance (PolymorphicSemiIsomorphism i) => PolymorphicIsomorphism (i Identity)
-}

newtype PackedMonoSemiIso m s a = PackedMonoSemiIso { _rawPackedMonoSemiIso :: (s -> m a, a -> m s) }

instance MonomorphicSemiIsomorphism PackedMonoSemiIso where
    packMonomorphicSemiIsomorphism :: Monad m => (s -> m a, a -> m s) -> PackedMonoSemiIso m s a
    packMonomorphicSemiIsomorphism = PackedMonoSemiIso
    unpackMonomorphicSemiIsomorphism :: Monad m => PackedMonoSemiIso m s a -> (s -> m a, a -> m s)
    unpackMonomorphicSemiIsomorphism = _rawPackedMonoSemiIso

{-  An instance MonomorphicIsomorphism (PackedMonoSemiIso Identity) is already supplied in
    the module Sky.Classes.Isomorphism.Monomorphic, via:
        instance (MonomorphicSemiIsomorphism i) => MonomorphicIsomorphism (i Identity)
-}

{-  An instance of Control.Category is already provided in the module
    Sky.Classes.SemiIsomorphism.Monomorphic, via:
    instance (Monad m, MonomorphicSemiIsomorphism i) => Category (i m)
-}

----------------------------------------------------------------------------------------------------
-- Isofunctor for monomorphic isomorphisms

{-  An instance IsoFunctor (PackedMonoSemiIso m x) is already supplied in
    the module Sky.Classes.IsoFunctor, via:
        instance (Monad m, MonomorphicSemiIsomorphism j) => IsoFunctor (j m x)
-}

{-  An instance ExposedIsoFunctor (PackedMonoSemiIso m x) is already supplied in
    the module Sky.Classes.IsoFunctor, via:
        instance (Monad m, MonomorphicSemiIsomorphism j) => ExposedIsoFunctor (j m x) where
-}

-- When an isofunctor also has an underlying monad m, SemiIsomorphisms with the same underlying monad m
--  can just be applied normally to it
semiisomap :: forall i f s a. (MonomorphicSemiIsomorphism i, ExposedIsoFunctor f) => i (ExposedM f) s a -> f s -> f a
semiisomap iso = let
    (sma, ams) = unpackMonomorphicSemiIsomorphism iso
    exposedIso :: PackedMonoSemiIso Identity ((ExposedM f) s) ((ExposedM f) a)   -- Iso (m s) (m a)
    exposedIso = packMonomorphicIsomorphism ((>>= sma), (>>= ams))
    in merge . isomap exposedIso . expose
