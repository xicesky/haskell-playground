
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}     -- For Exposed
{-# LANGUAGE MultiParamTypeClasses #-}      -- For Exposed
{-# LANGUAGE FlexibleContexts #-}           -- For Exposed (Either String) p
{-# LANGUAGE FlexibleInstances #-}          -- For Exposed m (ExchangeM m a b)

module Old.Sky.Isomorphism.OldSemiIso where

import Data.Functor.Identity
import qualified Control.Category as Cat
--import Control.Monad (join)
import Control.Monad ((>=>))

--import Old.Sky.Experiments

----------------------------------------------------------------------------------------------------

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap = dimap id

instance Profunctor (->) where
    --dimap :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
    dimap ab cd bc = cd . bc . ab
    lmap ab bc = bc . ab
    rmap bc ab = bc . ab

-- | Exposes structure of a Kleisli category beneath a profunctor.
-- 
-- Should obey laws:
-- 
-- prop> merge . rmap return = id
-- prop> lmap return . expose = id
-- prop> rmap (>>= f) = merge . rmap (fmap f)
-- prop> lmap (fmap f) . expose = expose . lmap f
class (Monad m, Profunctor p) => Exposed m p | p -> m where
    expose :: p a b -> p (m a) b
    merge  :: p a (m b) -> p a b 

----------------------------------------------------------------------------------------------------

-- We need a profunctor to "fetch" the functions out of isos
data Exchange a b s t = Exchange (s -> a) (b -> t)
--type Exchange' a s t = Exchange a a s t
--type Ex' a s = forall f. Functor f => Exchange a a s (f s)

instance Profunctor (Exchange a b) where
    dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)
    lmap f (Exchange sa bt) = Exchange (sa . f) bt
    rmap f (Exchange sa bt) = Exchange sa (f . bt)

----------------------------------------------------------------------------------------------------

-- Naive simple Iso
data IsoR' s a = IsoR' (s -> a) (a -> s)

-- Naive simple SemiIso: https://hackage.haskell.org/package/partial-isomorphisms
data SemiIsoR' s a = SemiIsoR' (s -> Maybe a) (a -> Maybe s)

-- The semi-iso package uses an Either String: https://hackage.haskell.org/package/semi-iso-1.0.0.0/docs/Control-Lens-SemiIso.html
data SemiIsoS' s a = SemiIsoS' (s -> Either String a) (a -> Either String s)

-- Our implementation should be free to use any monad (functors are not enough?)
data SemiIsoM' m s a = SemiIsoM' (s -> m a) (a -> m s)

instance Monad m => Cat.Category (SemiIsoM' m) where
    -- id :: SemiIsoM' a a
    id = SemiIsoM' return return
    -- (.) :: SemiIsoM' b c -> SemiIsoM' a b -> SemiIsoM' a c
    SemiIsoM' bmc cmb . SemiIsoM' amb bma = SemiIsoM' (amb >=> bmc) (cmb >=> bma)

----------------------------------------------------------------------------------------------------
-- Non-simple versions (for polymorphic types)

-- Naive Iso (= Exchange)
data IsoR s t a b = IsoR (s -> a) (b -> t)

-- Naive monadic semiiso
data SemiIsoM m s t a b = SemiIIsoM (s -> m a) (b -> m t)

----------------------------------------------------------------------------------------------------
-- Non-naive, i.e. function composable

-- Simple
-- type Iso' s a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type SemiIsoS s t a b = forall p f. (Exposed (Either String) p, Traversable f) => p a (f b) -> p s (f t)

type SemiIso m s t a b = forall p f. (Exposed m p, Traversable f) => p a (f b) -> p s (f t)

----------------------------------------------------------------------------------------------------
-- Implementation of our SemiIso

-- We need a profunctor to "fetch" the functions out of isos
data ExchangeM m a b s t = ExchangeM (s -> m a) (b -> m t)
-- semi-iso defines:
-- data Retail s t a b = Retail (a -> Either String s) (t -> Either String b)
-- which is exactly ExchangeM (Either String)
type Retail = ExchangeM (Either String)

instance Functor m => Profunctor (ExchangeM m a b) where
    dimap f g (ExchangeM sma bmt) = ExchangeM (sma . f) (fmap g . bmt)
    lmap f (ExchangeM sma bmt) = ExchangeM (sma . f) bmt
    rmap f (ExchangeM sma bmt) = ExchangeM sma (fmap f . bmt)

-- join from Control.Monad
join :: Monad m => m (m a) -> m a
join mma = mma >>= id

instance Monad m => Exposed m (ExchangeM m a b) where
    -- expose :: ExchangeM m x y a b -> ExchangeM m x y (m a) b
    expose (ExchangeM amx ymb) = ExchangeM (>>= amx) ymb    -- (m a -> m x, y -> m b)
    -- merge  :: ExchangeM m x y a (m b) -> ExchangeM m x y a b 
    merge (ExchangeM amx ymmb) = ExchangeM amx (join . ymmb)     -- (a -> m x, y -> m b)

semiIso :: (s -> m a) -> (b -> m t) -> SemiIso m s t a b
semiIso sa bt = merge . dimap sa (sequenceA . fmap bt) . expose

--exchangeZero :: Exchange a b a (Identity b)
--exchangeZero = Exchange id Identity

exchangeZero :: Monad m => ExchangeM m a b a (Identity b)     -- (a -> m a) (b -> m (Identity b))
exchangeZero = ExchangeM return (return . Identity)

--exampleIso :: (Traversable f, Exposed M p) => p A (f B) -> p S (f T)
-- exampleIso :: SemiIso M S T A B
-- exampleIso = semiIso s_ma b_mt

unpackSemiIso :: (Traversable m, Monad m) => SemiIso m s t a b -> (s -> m a, b -> m t)
unpackSemiIso iso = case iso exchangeZero of
    -- ExchangeM m a b s (Identity t)
    ExchangeM sma bmit -> (sma, rmap (runIdentity . sequenceA) bmit)

--applySemiIso :: SemiIso m s t a b -> s -> m a
--unapplySemiIso :: SemiIso m s t a b -> b -> m t
--withSemiIso :: SemiIso m s t a b ->  ((s -> ma) -> (b -> m t) -> r) -> r
--reverseSemiIso :: SemiIso m s t a b -> SemiIso m b a t s
