
{-
Interfaces for non-deterministic search.
-}
module NonDet.Class
    (   -- * Main definitions
        NonDet(..)
    ,   SFun(..)

    ,   -- * Example problems
        pytriple'
    ,   pidgeonHole
    ,   pidgeonHole'

    ,   -- * Using lists
        searchList

    ,   -- * Utilities
        guard
    ) where

import Data.List (foldl')
import Control.Monad (MonadPlus(..), guard)

{-----------------------------------------------------------------------------}
-- Support for non-determinism (apart from Applicative)

{- | Types supporting non-deterministic choice

This is basically the same as 'MonadPlus', but with additional
class members that can be implemented to improve efficiency.
-}
class MonadPlus m => NonDet m where
    {-# MINIMAL #-}

    failure :: m a
    failure = mzero

    choice :: m a -> m a -> m a
    choice = mplus

    choices :: [m a] -> m a
    choices = foldl' choice failure

    anyof :: [a] -> m a
    anyof = choices . fmap pure

    -- guard :: Bool -> m ()
    -- guard True  = return ()
    -- guard False = failure

{-----------------------------------------------------------------------------}

{- | Type of solver functions

Solver functions should implement this type, which requires that
problems be specified for an abstract 'NonDet m', not any specific
NonDet instance.
-}
newtype SFun = SFun { getFun :: forall a. (forall m. NonDet m => m a) -> [a] }

{-----------------------------------------------------------------------------}
-- Basic impl using lists

instance NonDet []

searchList :: SFun -- [a] -> [a]
searchList = SFun id

{-----------------------------------------------------------------------------}
-- Example problems

-- Pythagorean triples, limited to values <= x
pytriple' :: NonDet m => Int -> m (Int, Int, Int)
pytriple' x = do
    a <- anyof [1..x]
    b <- anyof [a+1..x]
    c <- anyof [b+1..x]
    guard $ a * a + b * b == c * c
    return (a, b, c)

-- Pidgeonhole, n into m
pidgeonHole :: NonDet m => Int -> Int -> m [Int]
pidgeonHole 0 _ = return []
pidgeonHole n m = do
    -- Choose hole for n-th pidgeon
    hole <- anyof [1..m]
    others <- pidgeonHole (n-1) m
    --traceM $ "pidgeonHole " ++ show (hole : others)
    -- Hole must be free
    guard $ notElem hole others
    return $ hole : others

{- | How to fit n pidgeons in (n-1) holes?

Will it ever work? :P

In ghci without optimisations (on my Macbook), @n = 8@ is already slow.
-}
pidgeonHole' :: NonDet m => Int -> m [Int]
pidgeonHole' n = pidgeonHole n (n-1)
