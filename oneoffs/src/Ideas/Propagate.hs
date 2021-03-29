
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}

{-
What does it mean to propagate information about a problem?
-}
module Ideas.Propagate where

import Control.Applicative
-- import qualified Data.Semilattice.Join as SL
-- import Data.Semilattice.Lower
-- import Data.Semilattice.Upper
import Optics
import Data.Tuple.Optics

{-
In general, a solveable problem has the form of a testable property:
-}
type TestProblem a = a -> Bool

{--------------------------------------------------------------------------------------------------}
{-
You could, of course, just generate a until the test succeeds.  That's just "brute force" solving,
and in general not feasible.

Instead, we want to "break down" the problem into smaller parts (constraints) that can be
individually tested. But those parts might interact.

Example in Sudoku: A number occurs exactly once in a specific row, but also exactly once in a
specific group.  If it occurs in a cell, then we need to check both uniqueness in the row and in the
cell.

Therefore we decompose our problems into "variables" that can be shared between constraints. Gaining
information about a variable (e.g. assigning a number, or eliminating one) can then trigger a
constraint (that actually "cares about" that variable) to become invalid.
-}
type ExampleProblem = (Int, Int, Int)

-- Check the domain of all variables
constraintDomain :: [ExampleProblem -> Bool]
constraintDomain = fmap check [_1, _2, _3] where
    -- Check the domain of variable at index @ix@
    check :: Lens' s Int -> s -> Bool
    check ix prob = let
        i = view ix prob
        in 0 <= i && i < 9

constraint01 :: ExampleProblem -> Bool
constraint01 p = (p ^. _1) < (p ^. _2)
--constraint01 (a, b, _) = a < b

constraint02 :: ExampleProblem -> Bool
constraint02 (_, b, c) = b < c

{--------------------------------------------------------------------------------------------------}
{-
Now we can generate parts of the problem domain, check the appropriate constraints, and backtrack on
failure ... right?

Only if we hardcode which constraints are being used! If we want to write code that works for a
general problem:

1. We don't know which constraints check which variables, so we'd have to check ALL of them, but
    then

2. Some constraints may fail on variables that we don't even care about yet!

We could just solve #2 and not care so much which constaints are actually checked. This works by
representing our problem as a join-semilattice (most likely composed of seperate join-semilattices
for each variable):
    https://en.wikipedia.org/wiki/Semilattice

This means each variable can be undetermined (bottom) and then "gain information", which makes the
the whole problem "gain information". Constraints then also produce an output lattice which can be
"undetermined" (bottom), "satisfied" (true) or "unsatisfied" (false).

(There is also a "top" value which stands for "conflict", for example when a variable is assigned
two different values at the same time, or no possible choice from a set is left.  Since this is
immediately invalid, we don't usually think about it and just "fail" if it actually happens.)

You can think of this as having all your variables @a@ replaced by @Maybe a@ or @Set a@ indicating
determined or possible values.
-}

type ExampleProblemL = (Maybe Int, Maybe Int, Maybe Int)

-- Check the domain of all variables
constraintDomainL :: [ExampleProblemL -> Maybe Bool]
constraintDomainL = fmap check [_1, _2, _3] where
    -- Check the domain of variable at index @ix@
    check :: Lens' s (Maybe Int) -> s -> Maybe Bool
    check ix prob = prob ^. ix & traversed %~ ok
        -- == over traversed ok (view ix prob)
        where
        ok :: Int -> Bool
        ok i = 0 <= i && i < 9

(<<) :: (Applicative f, Applicative g, Ord a) => f (g a) -> f (g a) -> f (g Bool)
(<<) = liftA2 $ liftA2 (<)

constraint01L :: ExampleProblemL -> Maybe Bool
constraint01L = (^. _1) << (^. _2)
    
constraint02L :: ExampleProblemL -> Maybe Bool
constraint02L = (^. _2) << (^. _3)

{--------------------------------------------------------------------------------------------------}
{-
NB: There can actually be multiple possible lattices for some types:

For Bool we can use:
    Maybe Bool  (semi-lattice)
    L Bool      (lattice)
    Set Bool    (lattice, equivalent to L Bool)

Those are almost equivalent, Set Bool just includes the "top" value:
            Neither
            /      \
        True        False
            \      /
            Either

For Int we can use:
    Maybe Int   (semi-lattice)
    Set Int     (lattice)
    Range Int   (lattice)

And this will limit how "useful" our constraints can be when we come to propagation, see note (N1)
below.

-}

{--------------------------------------------------------------------------------------------------}
{-
This is very general, because we can leave our problem structure as it is. But is also highly
/inefficient/ exactly because we will check constraints that are already satisfied or completely
irrelevant to the variables we gained information on.

To solve #1 we could add some structure to the constraints, so we can determine all the variables
that they check. This also requires that we can identify the variables in our problem /in general/,
for example by giving them names (indices).

So for a given problem domain P, we need to have functions that allow us to:
    - decompose it into smaller domains
    - turn these sub-domains into semilattices
    - re-compose the problem from determined values

And for constraints we need:
    - the set of sub-domains that the constraint operates on
    - a function to evaluate the constraint once all the variables are determined

One difficulty is representing variables of different types.  Say our problem has Boolean and
Integer constraints like:
    (x < y) && ((y < x) || (y == 0))

Then we have variables x and y that range over @Int@ and also expressions (which we will represent
by "anonymous" variables) of type @Bool@:
    x               :: Int
    y               :: Int
    a0 = (y < x)    :: Bool
    a1 = (y == 0)   :: Bool

    (x < y)         :: Constraint
    a0 || a1        :: Constraint

How do we represent the variables in a way that doesn't erase their types during the runtime of the
solver?

The solver doesn't really care about the types of the variables, it only needs to know when
variables are assigned (gain information) or conflict (which is also information gain).  But to
rebuild the (partial) solution, we'd like to be type-safe.  For this purpose we can use heterogenous
lists (see HList from the typelevel package).

But since our problem consists of nothing else but variables and constraints, why not represent the
whole problem like this in the first place?
-}

data Set a
data Variable a
data Constraint
data Problem

decompose :: Problem -> Set (Variable x)
decompose = undefined

{-
LessThan works on Ranges or Sets
-}

lessThan :: Variable Int -> Variable Int -> Constraint
lessThan = undefined

variablesOf :: Constraint -> Set (Variable x)
variablesOf = undefined

{--------------------------------------------------------------------------------------------------}
{-
There is one big possible optimization: Constraints that have n variables become "unit", when n-1
variables are determined.  This means they can determine the last variable for us, instead of us
having to try all the possibilities.

We can then go on to check the other constraints for the newly determined variable. This is called
propagation.

But how do we represent that using a functional interface?  E.g. For the constraint @x + y + z = 0@:
    Int 
    (x, Any) -> (x, -x)
    (Any, y) -> (-y, y)

We don't want to have functions for every possible "last missing variable"! Instead we can run such
constraints directly on the semilattice variables, which requires that our constraints actually
support semi-lattices, and we can't use arbitrary functions anymore.  For the user it would be
cumbersome to have to write such constraints, so we need to compose them out of basic building
blocks.

Note (N1) for choice of lattice: Depending on the lattice you choose, propagation might not be
possible. Choosing e.g.  @Maybe Int@ for a "less than" constraint @x < y@, even if we determine one
variable @y = 5@, we get @x < 5@, which we can't pin on any specific @x@. If we had chosen
@Range Int@ or @Set Int@ instead, we could have propagate some information!
-}

{-
But how to achieve efficient updates? Given a specific modification at position p in the structure
s, we'd like to only "run" the propagators that actually depend on the value at that position, e.g.:
    ((a, b, c), a -> c -> m z)
Since the function @a -> c -> mz@ doesn't depend on b, we'd like to avoid running it if we only
change b.

This would require us to represent structures as indexed containers like in
http://www.cs.nott.ac.uk/~psztxa/publ/jpartial.pdf where the positions are comparable. There are
multiple ways to implement this:

1. Establish an isomorphism from the structure s to a simpler structure, for example a heterogenous
    list or an /IxSet/ (see IxSet on hackage). Positions are then represented using indices or keys.

2. Establish some "positional" type for containers explicitly. We need to be able to compare two
    positions, so lenses or zippers by themselves will not do. So we can:

    2.1. Use lenses with some specific equality. (See ELens.hs for some ideas)
    2.2. Use generic zippers with a type that represents movements, for example:
        -- using https://michaeldadams.org/papers/scrap_your_zippers/
        data Dir = Down | Left | Here
    2.3. Use differentials of containers to get a path type.
        http://www.cs.nott.ac.uk/~psztxa/publ/jpartial.pdf

Method 1 preserves type safety for the user, but not our implementation (Indices might be out of
range). Method 2.2 also sacrifices some type safety but can be implemented as a type-safe interface
that would look similar to 2.1. Method 2.3 is perfectly type-safe but almost impossible to implement
in Haskell without dependent types (See DiffDS.hs).

(UpdateDS.hs describes another idea which might work but is incomplete right now.)
-}


class (Eq a, Eq (v a)) => LVar v a | v a -> a where
    -- Variable is a join-semilattice
    lAny :: v a
    lDetermined :: a -> v a
    lJoin :: MonadFail m => v a -> v a -> m (v a)

type Action m s = s -> m s

-- | The action of "setting a variable"
set :: (MonadFail m, LVar v a) => a -> Action m (v a)
set a = lJoin (lDetermined a)

class EVar v where
    eAny :: v
    -- default eAny :: LVar lv a => lv a
    -- eAny = lAny

    eJoin :: MonadFail m => v -> v -> m v

data EV = forall v a. LVar v a => EV { unEV :: v a }

-- This won't work
-- instance Eq EV where
--     (==) (EV a) (EV b) = a == b

-- Neither will this
--instance EVar EV where
    --eAny = EV lAny
    --eJoin = ??

instance Eq a => LVar Maybe a where
    lAny = Nothing
    lDetermined = Just
    lJoin (Just _) (Just _) = fail "Assignment"
    lJoin Nothing x = return x
    lJoin x Nothing = return x

instance Eq a => EVar (Maybe a) where
    eAny = lAny
    eJoin = lJoin
