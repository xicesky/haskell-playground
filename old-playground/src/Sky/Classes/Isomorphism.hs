
{-# LANGUAGE ScopedTypeVariables #-}

module Sky.Classes.Isomorphism
    ( MonomorphicIsomorphism
    , PolymorphicIsomorphism
    , MonomorphicSemiIsomorphism
    , PolymorphicSemiIsomorphism
    , packMonomorphicIsomorphism
    , unpackMonomorphicIsomorphism
    , toMonomorphicIsomorphism
    , applyMonomorphicIsomorphism
    , unapplyMonomorphicIsomorphism
    , revertMonomorphicIsomorphism
    , packPolymorphicIsomorphism
    , unpackPolymorphicIsomorphism
    , toPolymorphicIsomorphism
    , applyPolymorphicIsomorphism
    , unapplyPolymorphicIsomorphism
    , revertPolymorphicIsomorphism
    , packMonomorphicSemiIsomorphism
    , unpackMonomorphicSemiIsomorphism
    , toMonomorphicSemiIsomorphism
    , applyMonomorphicSemiIsomorphism
    , unapplyMonomorphicSemiIsomorphism
    , reverseMonomorphicSemiIsomorphism
    , packPolymorphicSemiIsomorphism
    , unpackPolymorphicSemiIsomorphism
    , toPolymorphicSemiIsomorphism
    , applyPolymorphicSemiIsomorphism
    , unapplyPolymorphicSemiIsomorphism
    , reversePolymorphicSemiIsomorphism
    , liftToSemiIsomorphism
    ) where

    --( module Sky.Classes.Isomorphism.Monomorphic
    --, module Sky.Classes.Isomorphism.Polymorphic
    --, module Sky.Classes.SemiIsomorphism.Polymorphic
    --, module Sky.Classes.SemiIsomorphism.Monomorphic
    --) where

import Sky.Classes.Isomorphism.Monomorphic
import Sky.Classes.Isomorphism.Polymorphic
import Sky.Classes.SemiIsomorphism.Monomorphic
import Sky.Classes.SemiIsomorphism.Polymorphic

import Data.Functor.Identity

-- Isomorphisms can be "lifted" to SemiIsomorphisms in the Identity Monad

liftToSemiIsomorphism :: forall i j m a b. (MonomorphicIsomorphism i, MonomorphicSemiIsomorphism j, Monad m) => i a b -> j m a b
liftToSemiIsomorphism iso = packMonomorphicSemiIsomorphism (amb, bma) where
    (ab, ba) = unpackMonomorphicIsomorphism iso
    amb :: a -> m b
    amb = return . ab
    bma :: b -> m a
    bma = return . ba
