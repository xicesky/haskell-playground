
{-| Fully operational semi-isomorphisms using the van Laarhoven representation.

    These can be used as:
        - Isomorphisms
        - Partial isormorphisms
        - Semi-Isomorphisms (see e.g. the semi-iso package)

    TODO:
        - Replace "Exchange" by "FunctionPair" with an actual tuple inside
        - Tests? Real proofs?

    Problems:
        - Need reified versions for storing in containers because the type
            [SemiIso ...] is illegal (requires ImpredicativeTypes)
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}     -- For Exposed
{-# LANGUAGE MultiParamTypeClasses #-}      -- For Exposed
--{-# LANGUAGE FlexibleContexts #-}           -- For Exposed (Either String) p
{-# LANGUAGE FlexibleInstances #-}          -- For Exposed m (Exchange m x y)
{-# LANGUAGE InstanceSigs #-}               -- Because i love it

module Sky.Isomorphism.SemiIso
    (   SemiIso
    ,   SemiIso'
    --,   Iso
    --,   Iso'
    ,   semiIso
    ,   unpackIso
    ,   packIso
    ,   applySemiIso
    ,   unapplySemiIso
    ,   reverseIso
    ) where

import Data.Functor.Identity
import Data.Tuple (swap)
import qualified Control.Category as Cat
--import Control.Monad (join)
import Control.Monad (join, (>=>))

import Sky.Isomorphism.Class

----------------------------------------------------------------------------------------------------
-- Definitions

-- A profunctor "p b c" can be seen as "some kind of computation from b to c"
class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap = dimap id

-- When a profunctor "p a b" exposes a monad m, it means it's really something like
-- a monadic computation "a -> m b"
class (Monad m, Profunctor p) => Exposed m p | p -> m where
    -- Taking a monad as input is not a problem, since we can "run" the monad
    expose :: p a b -> p (m a) b
    -- We can "hide" the fact that the output is monadic inside the profunctor
    merge  :: p a (m b) -> p a b

{- Whenever a profunctor p exposes the monad m, we can use dimap with monadic transformations.
    expose :: p b c -> p (m b) c
    dimap :: (a -> m b) -> (c -> m d) -> p (m b) c -> p a (m d)
    merge :: p a (m d) -> p a d
-}
dimapM :: forall m p a b c d. (Exposed m p) =>
    (a -> m b) -> (c -> m d) -> p b c -> p a d
dimapM amb cmd = merge . dimap amb cmd . expose

----------------------------------------------------------------------------------------------------
-- Our profunctor "function pairs to a monad m"

-- If the order of the type parameters seems a little weird: We need the "outer types" s and t
-- for the definition of "Profunctor" and "Exposed", so they are last in the definition.

data Exchange m a b s t = Exchange (s -> m a) (b -> m t)
type Exchange' m a s = Exchange m a a s s -- = Exchange (s -> m a) (a -> m s)

instance Monad m => Profunctor (Exchange m x y) where
    dimap :: (a -> b) -> (c -> d) -> (Exchange m x y) b c -> (Exchange m x y) a d
    dimap ab cd (Exchange bmx ymc) = Exchange (bmx . ab) (fmap cd . ymc)
    lmap :: (a -> b) -> (Exchange m x y) b c -> (Exchange m x y) a c
    lmap ab (Exchange bmx ymc) = Exchange (bmx . ab) ymc
    rmap :: (b -> c) -> (Exchange m x y) a b -> (Exchange m x y) a c
    rmap bc (Exchange amx ymb) = Exchange amx (fmap bc . ymb)

instance Monad m => Exposed m (Exchange m x y) where
    -- When the input is monadic, simply evaluate it first before applying the "towards" function
    expose :: (Exchange m x y) a b -> (Exchange m x y) (m a) b
    expose (Exchange amx ymb) = Exchange (>>= amx) {- :: m a -> m x) -} ymb
    -- When the output is monadic, it really means we have "m (m b)", which we can just join
    merge  :: (Exchange m x y) a (m b) -> (Exchange m x y) a b
    merge (Exchange amx ymmb) = Exchange amx (join . ymmb) {- :: y -> m b -}

exchangeZero :: Monad m => Exchange m a b a (Identity b)
exchangeZero = Exchange return (return . Identity)

----------------------------------------------------------------------------------------------------
-- Definiton of SemiIso

type SemiIso m s t a b = forall p f. (Exposed m p, Traversable f) => p a (f b) -> p s (f t)

{- Read it like this:
    A SemiIso (semi-isomorphism) takes a profunctor type p (with expose underlying monad m) and
    a traversable functor type f, and is then able to "lift" any such profunctor
    "p a (f b)" to a profunctor "p s (f t)" for some fixed types s, t, a and b.

    This means:
        - It only works for specific s, t, a, b
        - It works for any profunctor (as long as you have Exposed defined for them)
        - It works for any traverseable functor f
-}

-- The simple version for non-polymorphic stuff is:
type SemiIso' m s a = SemiIso m s s a a
--                  = forall p f. (Exposed m p, Traversable f) => p a (f a) -> p s (f s)

{- This simpler version is much easier to recognize:
    It "lifts" a computation on a type a to a computation on type s, modulo some monad m
    and traverseable functor f.
-}

-- When you don't need the monadic part, simply set m = Identity and you get an actual
-- isomorphism. (Sadly it would still require f to be traversable, which can be avoided in
-- this special case).
type Iso s t a b -- = SemiIso Identity s t a b
--                  = forall p f. (Profunctor p, Traversable f) => p a (f b) -> p s (f t)
                    = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
-- NB: This is a lens if you use "p = (->)" ;)
-- Compat: You may use an Iso in place of any SemiIso, but you can't use SemiIso as
--      an Iso (because of the restriction to Traverseable made by SemiIso).
--      This is actually what our intuition says.

-- And, again, we have the non-polymorphic version:
type Iso' s a       = Iso s s a a
--                  = forall p f. (Profunctor p, Functor f) => p a (f a) -> p s (f s)

----------------------------------------------------------------------------------------------------
-- Creating SemiIsos

{- How to build a SemiIso:

This function takes the forward and backward transformations for a semiiso and produces a
transformation on our Profunctor p a (f b).

EX: If those transformations and our profunctor were "ordinary functions", this would mean taking
    - The "towards transformation"  :: s -> a
    - The "back transformation"     :: b -> t
    - And the "inner computation"   :: a -> b
    And composing them in the obvious way:
        (b -> t) . (a -> b) . (s -> a)

Now doing the "same" thing with our profunctors p and traverseable functors f means:
    - The "towards transformation" is some monadic computation
            :: s -> m a
    - The "back transformation" is again a monadic computation
            :: b -> m t
    - And our "inner computation" creates some kind of traversable structure f
            :: p a (f b)    (think of a -> f b)

The inner computation is now a profunctor, and thus we need to use "dimap" to apply our
transformations before and after the computation:
    dimap :: (s -> a) -> (b -> t) -> p a b -> p s t

Our inner computation "can deal" with monad m by use dimapM (see above):
    dimapM :: (a -> m a) -> (b -> m t) -> p a b -> p s t

Now we add in some additional (traversable) structure f, which is generated by the inner
computation, which we "hand through" to the eventual output "t":
    dimapM :: (a -> m a) -> (f b -> m (f t)) -> p a (f b) -> p s (f t)

How do we actually run the "back transformation" "b -> m t" on some structure "f b"?
First we fmap the transformation on the whole structure:
    fmap :: (b -> m t) -> f b -> f (m t)
And the sequence all the computations inside that structure (that's why it's a traversable):
    sequenceA :: (Applicative m, Traversable f) => f (m t) -> m (f t)

-}

-- semiIso sa bt = merge . dimap sa (sequenceA . fmap bt) . expose
-- semiIso sa bt = \x :: p a (f b) -> merge (dimap sa (sequenceA . fmap bt) (expose x))
semiIso :: forall m s t a b p f.
    (Exposed m p, Traversable f) =>
    (s -> m a) -> (b -> m t) -> p a (f b) -> p s (f t)
semiIso sma bmt innerComputation = liftedComputation where
    -- fmap :: (Functor f) => (b -> m t) -> f b -> f (m t)
    -- sequenceA :: (Applicative m, Traversable f) => f (m t) -> m (f t)
    transformBack :: Traversable f => f b -> m (f t)
    transformBack = sequenceA . (fmap bmt)

    -- dimapM :: (s -> m a) -> (f b -> m (f t)) -> p a (f b) -> p s (f t)
    liftedComputation :: p s (f t)
    liftedComputation = dimapM sma transformBack innerComputation


-- This can be written very short, as:
--semiIso sma bmt = dimapM sma (sequenceA . fmap bmt)

----------------------------------------------------------------------------------------------------
-- Deconstruction of a SemiIso

{- How to deconstruct a SemiIso:

We use our custom profunctor "Exchange m a b", using exchangeZero as input to our SemiIso.
exchangeZero represents a "inner computation" that really does nothing, it contains two
functions "a -> m a" and "b -> m (Identity b)":
    exchangeZero :: Monad m => Exchange m a b a (Identity b)
    exchangeZero = Exchange return (return . Identity)

Applying our SemiIso to that will yield the same type of profunctor (!) (and the same functor),
but change the outer types to "s" and "t":
    siso :: (Exchange m a b) a (Identity b) -> (Exchange m a b) s (Identity t)

Which means it applied the forward and backward transformations to our identity functions,
thus yielding exactly those forward and backward transformations!

Proof:
        (semiIso sma bmt) exchangeZero
    =   dimapM sma (sequenceA . fmap bmt) (Exchange return (return . Identity))
    =   (merge . dimap sma (sequenceA . fmap bmt) . expose) (Exchange return (return . Identity))
    =   merge $ dimap sma (sequenceA . fmap bmt) (expose (Exchange return (return . Identity))
    =   merge $ dimap sma (sequenceA . fmap bmt) (Exchange (>>= return) (return . Identity))
    =   merge $ Exchange ((>>= return) . sma)  (fmap (sequenceA . fmap bmt) . (return . Identity))
    =   merge $ Exchange sma                  $ \x -> fmap (sequenceA . fmap bmt) (return $ Identity x)
    -- fmap f (return x) = return (f x)
    =   merge $ Exchange sma                  $ \x -> return (sequenceA (fmap bmt (Identity x)))
    -- fmap f (Identity x) = Identity (f x)
    =   merge $ Exchange sma                  $ \x -> return (sequenceA (Identity $ bmt x))
    -- sequenceA (Identity $ bmt x) = fmap Identity $ bmt x
    =   merge $ Exchange sma                  $ \x -> return (fmap Identity $ bmt x)
    =   Exchange sma $  join . (\x -> return (fmap Identity $ bmt x))
    =   Exchange sma $  \x -> join (return (fmap Identity $ bmt x))
    =   Exchange sma $  \x -> (return (fmap Identity $ bmt x)) >>= id
    -- return a >>= k = k a
    =   Exchange sma $  \x -> id (fmap Identity $ bmt x)
    =   Exchange sma $  \x -> fmap Identity $ bmt x

This means that our second function still is of type "b -> m (Identity t)", which we decode
by simply using "fmap runIdentity" on it

-}

unpackIso :: forall m s t a b. Monad m => SemiIso m s t a b -> (s -> m a, b -> m t)
unpackIso siso = let
    -- exchange :: Exchange a b s (Identity t)
    sma :: s -> m a
    bmit :: b -> m (Identity t)
    Exchange sma bmit = siso exchangeZero

    -- "Decode" m (Identity t)
    bmt :: b -> m t
    bmt = fmap runIdentity . bmit

    -- And we are done already
    in (sma, bmt)

----------------------------------------------------------------------------------------------------
-- Let's add a few helpers

-- The actual inverse of unpackSemiIso (semiIso doesn't work on tuples)
packIso :: forall m s t a b. Monad m => (s -> m a, b -> m t) -> SemiIso m s t a b
packIso (sma, bmt) = semiIso sma bmt

-- Apply a semiiso (this is just a part of unpackSemiIso)
-- This could be "fst . unpackSemiIso", but this is more efficient:
applySemiIso :: forall m s t a b. Monad m => SemiIso m s t a b -> s -> m a
applySemiIso siso = let
    sma :: s -> m a
    bmit :: b -> m (Identity t)
    Exchange sma bmit = siso exchangeZero
    in sma

-- Unapply, a.k.a. apply in reverse (this is just the other part of unpackSemiIso)
unapplySemiIso :: forall m s t a b. Monad m => SemiIso m s t a b -> b -> m t
unapplySemiIso siso = let
    sma :: s -> m a
    bmit :: b -> m (Identity t)
    Exchange sma bmit = siso exchangeZero
    in fmap runIdentity . bmit
-- unapply = apply . reverseSemiIso

-- Reverse a SemiIso
-- If you think of it as a tuple (s -> m a, b -> m t), then reverse = swap
reverseIso :: forall m s t a b. Monad m => SemiIso m s t a b -> SemiIso m b a t s
--reverseSemiIso :: forall m s t a b. Monad m =>
--        forall p f. (Exposed m p, Traversable f) => p a (f b) -> p s (f t)
--    ->  forall p f. (Exposed m p, Traversable f) => p t (f s) -> p b (f a)
reverseIso = packIso . swap . unpackIso
-- TODO: Can we make this more efficient somehow?

----------------------------------------------------------------------------------------------------

newtype ReifiedSemiIso m s t a b = ReifiedSemiIso { _rawSemiIso :: SemiIso m s t a b }

instance SemiIsomorphism ReifiedSemiIso where
    packSemiIsomorphism :: forall m s t a b. (Monad m) => (s -> m a, b -> m t) -> ReifiedSemiIso m s t a b
    packSemiIsomorphism tupl = ReifiedSemiIso (packIso tupl)
    unpackSemiIsomorphism :: forall m s t a b.  (Monad m) => ReifiedSemiIso m s t a b -> (s -> m a, b -> m t)
    unpackSemiIsomorphism iso = unpackIso (_rawSemiIso iso)

----------------------------------------------------------------------------------------------------
-- Maybe we want a few simple versions where "m" is just Identity?

