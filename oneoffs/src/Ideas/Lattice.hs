
module Ideas.Lattice
    (   L(..)
    ,   liftProp
    ,   -- * Re-exports
        (\/), (/\)
    ) where

import Data.Semilattice.Join
import Data.Semilattice.Meet
import Data.Semilattice.Lower
import Data.Semilattice.Upper

-- | Bounded join-semilattice class
class (Join a, Lower a, Upper a) => BJSemilattice a where
    -- No impl needed

{- | Simple flat lattice

Makes a full, "flat" lattice of any type supporting Eq.
-}
data L a
    = Determined a
    | Any       -- ^ Free choice / Full domain of a / Bottom
    | None      -- ^ Conflicting / Empty set / Top
    deriving (Eq, Ord, Functor, Foldable, Traversable)

-- Useful??
liftProp :: (a -> Bool) -> L a -> L ()
liftProp _ Any  = Any       -- We don't know yet
liftProp _ None = None      -- Impossible
liftProp f (Determined a)
    | f a   = Determined () -- Match
    | otherwise = None      -- Fail

{- 
FIXME is this even correct?
Basically, we want to apply functions only to determined values.
-}
instance Applicative L where
    pure = Determined
    Determined f <*> a      = fmap f a      -- Is this even ok for "Any"? We should gain info
    -- 'None' acts like 'Nothing' from Maybe
    None         <*> _      = None
    _            <*> None   = None          -- A lazy function might actually still give us sth
    -- 'Any' is kinda bad for functions...
    Any          <*> _      = Any           -- !??? Any <*> Determined

instance Eq a => Join (L a) where
    Determined a \/ Determined b
        | a == b    = Determined a
        | otherwise = None
    Any \/ x = x
    None \/ x = None
    x \/ Any = x
    x \/ None = None

instance Eq a => Meet (L a) where
    Determined a /\ Determined b
        | a == b    = Determined a
        | otherwise = Any
    Any /\ x    = Any
    None /\ x   = x
    x /\ Any    = Any
    x /\ None   = x

instance Lower (L a) where
    lowerBound = Any

instance Upper (L a) where
    upperBound = None

instance Eq a => BJSemilattice (L a)
