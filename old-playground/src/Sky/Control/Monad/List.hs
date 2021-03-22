
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}

{-| List "monad transformer
    Non-deterministic computation yieling all possible results.

    Problem: While List really is a monad, ListT is not one (see the Monad instance).
    The "bind" method would cannot work in the usual way, because this:
        (>>=) :: [m a] -> (a -> [m b]) -> [m b]
    Means that the size of the set of results depends on the input value (m a) und thus
    the previous computations.

    This could be resolved by making ListT of the type "m [m a]" but this way we do not
    get any "ensurance" that the computational context of the list elements doesn't affect
    the outer context (which only has to be there because of the list length).

    We need underlying monads that can deal with finite sets, e.g.:
        bindL :: (Set s) => m a => (a -> Set (m b)) -> Set (m b)

    Maybe we could even replace "Set" with different Functors:
        bindL :: (Functor f) => m a => (a -> f (m b)) -> f (m b)
    i.e.: Monads that can deal with arbitrary structure.

    Or we could think of a "Set" type that represents finite sets of unknown size. This
    would mean that the union would have to work on sets "not already available", like:
        transform :: Set (m a) -> (a -> (Set (m b)) -> Set (m b)

    Here, clearly, m needs to have some constraints, so that the set function can work with
    it, such as m being a monad.

-}

module Sky.Control.Monad.List where

import Control.Monad.Trans.Class

----------------------------------------------------------------------------------------------------

newtype ListT m a = ListT { runListT :: [m a] }

instance (Functor m) => Functor (ListT m) where
    fmap f (ListT ma) = ListT $ map (fmap f) ma     -- = fmap (fmap f) ma

instance (Applicative m) => Applicative (ListT m) where
    pure a = ListT $ [pure a]   -- = pure (pure a)
    -- (<*>) :: [m (a -> b)] -> [m a] -> [m b]
    (ListT fs) <*> (ListT xs) = ListT $ [f <*> x | f <- fs, x <- xs]    -- = ListT $ (fmap (\f -> (f <*>) fs) <*> xs
    -- (*>) :: [m a] -> [m b] -> [m b]
    -- TODO

instance (Monad m) => Monad (ListT m) where
    fail _ = ListT $ []
    --(>>=) :: [m a] -> (a -> [m b]) -> [m b]
    (ListT xs) >>= f = ListT $ [y | x <- xs, y <- doShit x (runListT . f) ] where
        doShit :: m a -> (a -> [m b]) -> [m b]
        doShit = undefined -- impossible
    {-
    -- What we would like to do is something like this:
    seqapp :: [m a] -> (a -> m b) -> [m b]
    seqapp xs f = fmap (>>= f) xs
    -- or
    multiseqapp :: [m a] -> [a -> m b] -> [m b]
    multiseqapp mas fs = [ma >>= f | ma <- mas, f <- fs]
    -- but that does not a monad make...
    -}

-- Find a solution for:
doAny :: forall a b. IO a -> (a -> [IO b]) -> [IO b]
-- ie: Having a computation that yields an a and a set of computations that yield b, but depend on a
doAny ma f = let
    doOne :: ([IO b] -> [IO b]) -> [IO b]
    doOne ptr = (ma >>= (head . ptr . f)) : (doOne (tail . ptr))    -- DOES NOT TERMINATE
    -- Problem: The list length depends on a, which is produced by an IO action
    --  which cannot yield "just a plain list"!
    in doOne id

-- (a -> [IO b]) = m1 (m2 (m3 b)) where m1 = (->) a, m2 = ([]), m3 = IO
