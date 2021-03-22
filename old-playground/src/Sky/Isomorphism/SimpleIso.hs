
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sky.Isomorphism.SimpleIso where

import Data.Functor.Identity
import Data.Monoid (Endo(..))

-- Naive definition: IsoR
data IsoR s a = IsoR { applyIsoR :: s -> a, unapplyIsoR :: a -> s }
-- = Exchange a a s s

unpackIsoR :: IsoR s a -> (s -> a, a -> s)
unpackIsoR (IsoR sa as) = (sa, as)

packIsoR :: (s -> a, a -> s) -> IsoR s a
packIsoR (sa, as) = IsoR sa as



----------------------------------------------------------------------------------------------------
-- Simplifications

class SimpleProfunctor p where
    dimap' :: (a -> b) -> (b -> a) -> p b -> p a

newtype FPair a s = FPair { _unpackFPair :: (s -> a, a -> s) }

instance SimpleProfunctor (FPair x) where
    --dimap' :: (a -> b) -> (b -> a) -> FPair x b -> FPair x a
    dimap' f g (FPair (bx, xb)) = FPair (bx . f, g . xb)
    -- Laws:
    -- dimap' id id ≡ id

-- Btw all endomorphisms are SimpleProfunctors
instance SimpleProfunctor Endo where
    dimap' ab ba (Endo bb) = Endo (ba . bb . ab)

----------------------------------------------------------------------------------------------------

-- "Cool" definition: SimpleIso
type SimpleIso s a = forall p. (SimpleProfunctor p) => p a -> p s
-- !! These can be conveniently combined using (.)

-- So when we use FPair as our profunctor, consider:
-- FPair a a -> FPair a s
-- i.e.:    p = FPair a
-- where    FPair (id, id) :: FPair a a
fPairZero :: FPair a a
fPairZero = FPair (id,id)

simpleIso :: (s -> a) -> (a -> s) -> SimpleIso s a
simpleIso sa as = dimap' sa as
-- simpleIso id id = dimap' id id = id  -- by definition of dimap'

unpackSimpleIso :: SimpleIso s a -> (s -> a, a -> s)
unpackSimpleIso iso = _unpackFPair (iso fPairZero)

packSimpleIso :: (s -> a, a -> s) -> SimpleIso s a
packSimpleIso (sa, as) = dimap' sa as
-- = simpleIso sa as

-- Our isos are already a category, because they are just functions
-- and category is already defined for functions
--instance Category Iso where
--    (.) f g = Base.(.) f g
--    id = Base.id -- = simpleIso id id

-- What happens if we use Endo as our profunctor?
-- Endo a -> Endo s
-- This is a bit like a lens, but can only modify!

----------------------------------------------------------------------------------------------------
-- Full version

-- Definition of Profunctor
-- we don't want to use the profunctors lib for now

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap = dimap id

-- We need a profunctor to "fetch" the functions out of isos
data Exchange a b s t = Exchange (s -> a) (b -> t)
--type Exchange' a s t = Exchange a a s t
--type Ex' a s = forall f. Functor f => Exchange a a s (f s)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
  lmap f (Exchange sa bt) = Exchange (sa . f) bt
  rmap f (Exchange sa bt) = Exchange sa (f . bt)

----------------------------------------------------------------------------------------------------

-- "Cool" definition: Iso'
type Iso' s a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)
-- !! These can be conveniently combined using (.)

-- Note for Control.Lens:
-- Iso' s a = Iso s s a a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

iso' :: (s -> a) -> (a -> s) -> Iso' s a
iso' sa as = dimap sa (fmap as)

-- unpackIso' :: Iso' s a -> (s -> a, a -> s)
-- When we set f = Identity and p = Exchange x x in Iso', we get:
-- (Exchange x x) a (Identity a) -> (Exchange x x) s (Identity s)
-- We'd like a "Exchange a _ s _", so we need x to be a!

exchangeZero :: Exchange a b a (Identity b)
exchangeZero = Exchange id Identity

unpackIso' :: Iso' s a -> (s -> a, a -> s)
unpackIso' iso = case iso exchangeZero of
    Exchange sa ais -> (sa, runIdentity . ais)

packIso' :: (s -> a, a -> s) -> Iso' s a
packIso' (sa, as) = iso' sa as

--applyIso' :: (Exchange' a a (Identity a) -> Exchange' a s (Identity s)) -> (s -> a)
applyIso' :: Iso' s a -> s -> a
applyIso' iso = case iso exchangeZero of
    Exchange sa ais -> sa

--unapplyIso' :: (Exchange' a a (Identity a) -> Exchange' a s (Identity s)) -> (a -> s)
unapplyIso' :: Iso' s a -> a -> s
unapplyIso' iso = case iso exchangeZero of
    Exchange sa ais -> runIdentity . ais

isoRtoIso' :: IsoR s a -> Iso' s a
isoRtoIso' = packIso' . unpackIsoR

iso'toIsoR :: Iso' s a -> IsoR s a
iso'toIsoR = packIsoR . unpackIso'
