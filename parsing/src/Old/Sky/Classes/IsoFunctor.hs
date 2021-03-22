
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}      -- IsoFunctorStageTwo
{-# LANGUAGE TypeFamilies #-}

module Old.Sky.Classes.IsoFunctor where

import Old.Sky.Classes.Isomorphism (liftToSemiIsomorphism)
import Old.Sky.Classes.Isomorphism.Monomorphic
--import Sky.Classes.Isomorphism.Polymorphic
import Old.Sky.Classes.SemiIsomorphism.Monomorphic
--import Sky.Classes.SemiIsomorphism.Polymorphic
--import Sky.Implementations.Isomorphism

import qualified Control.Category as Cat
import Control.Monad (join)
--import Data.Functor.Identity

----------------------------------------------------------------------------------------------------
-- Normal isomorphisms

-- Monomorphic
class IsoFunctor f where
    isomap :: forall i s a. (MonomorphicIsomorphism i) => i s a -> f s -> f a

-- Polymorphic (not ever needed, we use Profunctor instead)
--class PolymorphicIsoFunctor f where
--    polymorphicIsofunctorMap :: forall i s s' a a'. (PolymorphicIsomorphism i) => i s s' a a' -> f s s' -> f a a'

{- This is just a Profunctor with its arguments switched. IE:
    class Profunctor p where
        dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
        dimap f g = lmap f . rmap g

    When you unpack the isomorphism:
        (sa, a's') = unpack i
    And apply it via dimap:
        dimap a's' sa :: p s' s -> p a' a
-}

----------------------------------------------------------------------------------------------------
-- SemiIso

-- Monomorphic
class (Monad (ExposedM f), IsoFunctor f) => ExposedIsoFunctor f where
    type ExposedM f :: * -> *
    expose :: f a -> f ((ExposedM f) a)     -- f a -> f (m a)
    merge :: f ((ExposedM f) a) -> f a      -- f (m a) -> f a
    --sublime text = fuck you
{-
class (Monad (ExposedM p), Profunctor p) => Exposed p where
    type ExposedM p :: * -> *
    expose :: p a b -> p ((ExposedM p) a) b
    merge  :: p a ((ExposedM p) b) -> p a b 
-}

-- Not needed
{-
class SemiIsoFunctor f where
    type ExposedM f :: * -> *
    semiisomap :: forall i s a. (MonomorphicSemiIsomorphism i) => i (ExposedM f) s a -> f s -> f a
    --sublime text = fuck you

-- Could just be implemented by:
instance (ExposedIsoFunctor f) => SemiIsoFunctor f where
    type ExposedM f = ExposedM_ f
    semiisomap :: forall i s a. (MonomorphicSemiIsomorphism i) => i (ExposedM f) s a -> f s -> f a
    semiisomap iso = let
        (sma, ams) = unpackMonomorphicSemiIsomorphism iso
        exposedIso :: PackedMonoSemiIso Identity ((ExposedM f) s) ((ExposedM f) a)   -- Iso (m s) (m a)
        exposedIso = packMonomorphicIsomorphism ((>>= sma), (>>= ams))
        in merge . isomap exposedIso . expose

-- You can find a definition of semiisomap in Sky.Implementations.Isomorphism
-}

-- The polymorphic case is, again, covered by Profunctor and Exposed

----------------------------------------------------------------------------------------------------
-- All monomorphic isomorphisms can be used as isofunctors

{- This would overlap with the next one:
instance (Cat.Category j, MonomorphicIsomorphism j) => IsoFunctor (j x) where
    isomap :: forall i s a. (MonomorphicIsomorphism i) => i s a -> j x s -> j x a
    isomap isoa isob = (Cat..) (convertMonomorphicIsomorphism isoa) isob
-}

instance (Monad m, MonomorphicSemiIsomorphism j) => IsoFunctor (j m x) where
    isomap :: forall i s a. (MonomorphicIsomorphism i) => i s a -> j m x s -> j m x a
    isomap isoa isob = (Cat..) (liftToSemiIsomorphism isoa) isob

instance (Monad m, MonomorphicSemiIsomorphism j) => ExposedIsoFunctor (j m x) where
    type ExposedM (j m x) = m
    
    expose :: forall a. j m x a -> j m x (m a)      -- f a -> f (m a)
    expose iso = packMonomorphicSemiIsomorphism (xmma, mamx) where
        (xma, amx) = unpackMonomorphicSemiIsomorphism iso
        xmma :: x -> m (m a)
        xmma = return . xma                 -- This means to give "them" the full context, e.g. "Just (Nothing)"
        --xmma x = fmap return (xma x)      -- This would mean to give "them" a value in a fresh context, e.g. "Nothing"
        -- xmma x = xma x >>= (return . return)       -- The same as fmap, but harder to read
        mamx :: m a -> m x
        mamx = (>>= amx)
    
    merge :: forall a. j m x (m a) -> j m x a       -- f (m a) -> f a
    merge iso = packMonomorphicSemiIsomorphism (xma, amx) where
        (xmma, mamx) = unpackMonomorphicSemiIsomorphism iso
        xma :: x -> m a
        xma = join . xmma
        amx :: a -> m x
        amx = mamx . return
