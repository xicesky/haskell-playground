
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Sky.Classes.Isomorphism.Monomorphic where

import Sky.Classes.SemiIsomorphism.Monomorphic
import Data.Tuple (swap)
import Data.Functor.Identity
--import Control.Category (Category)
--import qualified Control.Category as Cat

class MonomorphicIsomorphism i where
    -- TODO: Provide default impl based on toMonomorphicIsomorphism
    packMonomorphicIsomorphism :: (s -> a, a -> s) -> i s a
    -- TODO: Provide default impl based on applyMonomorphicIsomorphism, unapplyMonomorphicIsomorphism
    unpackMonomorphicIsomorphism :: i s a -> (s -> a, a -> s)
    -- Derived (Will be alternatives for the above, when TODOs are finished)
    toMonomorphicIsomorphism :: (s -> a) -> (a -> s) -> i s a
    toMonomorphicIsomorphism sma bmt = packMonomorphicIsomorphism (sma, bmt)
    applyMonomorphicIsomorphism :: i s a -> s -> a
    applyMonomorphicIsomorphism = fst . unpackMonomorphicIsomorphism
    unapplyMonomorphicIsomorphism :: i s a -> a -> s
    unapplyMonomorphicIsomorphism = snd . unpackMonomorphicIsomorphism
    -- Derived
    revertMonomorphicIsomorphism :: i s a -> i a s
    revertMonomorphicIsomorphism = packMonomorphicIsomorphism . swap . unpackMonomorphicIsomorphism
    convertMonomorphicIsomorphism :: (MonomorphicIsomorphism j) => j s a -> i s a
    convertMonomorphicIsomorphism = packMonomorphicIsomorphism . unpackMonomorphicIsomorphism

----------------------------------------------------------------------------------------------------
-- Operators and stuff

iso :: forall i s a. (MonomorphicIsomorphism i) => (s -> a) -> (a -> s) -> i s a
iso = toMonomorphicIsomorphism

apply :: forall i s a. (MonomorphicIsomorphism i) => i s a -> s -> a
apply = applyMonomorphicIsomorphism

unapply :: forall i s a. (MonomorphicIsomorphism i) => i s a -> a -> s
unapply = unapplyMonomorphicIsomorphism

convert :: forall i j s a. (MonomorphicIsomorphism i, MonomorphicIsomorphism j) => i s a -> j s a
convert = convertMonomorphicIsomorphism

----------------------------------------------------------------------------------------------------
-- Any monomorphic Semi-Isomorphism is just an isomorphism when we use the identity monad

instance (MonomorphicSemiIsomorphism i) => MonomorphicIsomorphism (i Identity) where
    packMonomorphicIsomorphism :: forall s a. (s -> a, a -> s) -> i Identity s a
    packMonomorphicIsomorphism (sa, as) = packMonomorphicSemiIsomorphism (sma, ams) where
        sma :: s -> Identity a
        sma = return . sa
        ams :: a -> Identity s
        ams = return . as

    unpackMonomorphicIsomorphism :: forall s a. i Identity s a -> (s -> a, a -> s)
    unpackMonomorphicIsomorphism iso = (sa, as) where
        (sma, ams) = unpackMonomorphicSemiIsomorphism iso
        sa :: s -> a
        sa = runIdentity . sma
        as :: a -> s
        as = runIdentity . ams

----------------------------------------------------------------------------------------------------
-- Monomorphic isomorphisms can be made an actual instance of category
-- but you can't write it as a general rule like:

--instance MonomorphicIsomorphism i => Category i where
--    id :: i a a
--    id = packMonomorphicIsomorphism (id, id)

--    (.) :: i b c -> i a b -> i a c
--    (.) isoBC isoAB = isoAC where
--        (bc, cb) = unpackMonomorphicIsomorphism isoBC
--        (ab, ba) = unpackMonomorphicIsomorphism isoAB
--        ac = bc . ab
--        ca = ba . cb
--        isoAC = packMonomorphicIsomorphism (ac, ca)
