
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE TypeSynonymInstances #-}

module Old.Sky.Implementations.Isomorphism.MonoIso where

import Old.Sky.Classes.Isomorphism
import Old.Sky.Classes.Isomorphism.Monomorphic
import Old.Sky.Classes.IsoFunctor
import Old.Sky.Implementations.Isomorphism

import Data.Functor.Identity
import Control.Category (Category)
import qualified Control.Category as Cat

----------------------------------------------------------------------------------------------------
-- Commonly used isomorphisms

type Iso a b = PackedMonoSemiIso Identity a b

isoConst :: a -> b -> Iso a b
isoConst a b = iso (const b) (const a)

isoCons :: Iso (a, [a]) [a]
isoCons = iso (\(x,xs) -> x:xs) (\(x:xs) -> (x,xs))

isoAlternative :: forall a. (a -> Bool) -> Iso (Either a a) a
isoAlternative decision = iso joinAlt splitAlt where
    joinAlt :: Either a a -> a
    joinAlt (Left a) = a
    joinAlt (Right a) = a
    splitAlt x = if decision x then Left x else Right x

isoAlt :: forall a. (a -> Bool) -> Iso (Either a a) a
isoAlt = isoAlternative

--isoEither :: Iso a b -> Iso (Either a b) (a, Bool)

isoFixedRight :: b -> Iso (a,b) a
isoFixedRight fixed = iso (\(a,b) -> a) (\a -> (a, fixed))

isoFixedLeft :: a -> Iso (a,b) b
isoFixedLeft fixed = iso (\(a,b) -> b) (\b -> (fixed, b))

--isoSwap :: Iso (Either a b) (Either b a)
--isoRebalance :: Iso (Either (Either a b) c) (Either a (Either b c))

--isoSum :: Iso a b -> Iso c d -> Iso (Either a b) (Either c d)
--endoIso :: Iso a b -> Iso (Either a b) (Either a b)

-- "Functors preserve isomorphisms"
--isoFun :: Functor f => Iso a b -> Iso (f a) (f b)
