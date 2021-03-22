
{-
    I like this: https://hackage.haskell.org/package/enumerable-0.0.3/docs/Data-Enumerable.html
    But operations can easily go undecidable.
    We could create "terms" for sets of values of any enumerable type instead of using lazy
    evaluation, which allows us to express infinite sets in a finite form.
-}

module EnumerableTerm where

----------------------------------------------------------------------------------------------------
-- Internals

data Term a
    = FiniteSet Integer [a]     -- The integer serves as finiteness proof
    | NegativeSet { all :: [a], subtrahend :: Term a }  -- Read: Everything minus subtrahend

internalFinite :: [a] -> Term a
internalFinite vs = FiniteSet (length vs) vs

----------------------------------------------------------------------------------------------------

finiteLength :: Term a -> Maybe Integer
finiteLength (FiniteSet l _)    = Just l
finiteLength (NegativeSet _)    = Nothing

isFinite :: Term a -> Bool
isFinite = isJust . finiteLength

noneTerm :: Term a
noneTerm = FiniteSet []

singleTerm :: a -> Term a
singleTerm a = FiniteSet 1 [a]

{- Fixme: Finite for finitely enumerable types
allTerm :: Enumerable a => Term a
allTerm = NegativeSet enumerate noneTerm
-}

class AllTerm a where
    allTerm :: Term a
    -- =

instance (FinitelyEnumerable a) => AllTerm a where
    allTerm = FiniteSet cardinality enumerate

-- Overlap :(
instance (Enumerable a) => AllTerm a where
    allTerm = NegativeSet enumerate noneTerm

unionTerm :: Term a -> Term a -> Term a
unionTerm (NegativeSet all sa) (NegativeSet _ sb)           = NegativeSet all (intersectionTerm sa sb)
unionTerm (NegativeSet all sa) r@(FiniteSet lb b)           = NegativeSet all (sa `differenceTerm` r)
unionTerm (FiniteSet la a) (FiniteSet lb b)                 = internalFinite (union a b)
unionTerm x y = unionTerm y x                   -- Lazyness hack!

intersectionTerm :: Term a -> Term a -> Term a
intersectionTerm (NegativeSet all sa) (NegativeSet _ sb)    = NegativeSet all (unionTerm sa sb)
intersectionTerm (NegativeSet all sa) (FiniteSet bs)        = internalFinite (bs \\ sa)
intersectionTerm (FiniteSet la a) (FiniteSet lb b)          = internalFinite (intersection a b)
intersectionTerm x y = intersectionTerm y x     -- Lazyness hack!

differenceTerm :: Term a -> Term a -> Term a
differenceTerm (NegativeSet all sa) (NegativeSet _ sb)      = sb `differenceTerm` sa
differenceTerm (NegativeSet all sa) (FiniteSet bs)          = NegativeSet all (sa `unionTerm` bs)
differenceTerm (FiniteSet la a) (FiniteSet lb b)            = internalFinite (difference a b)

symmetricDifferenceTerm :: Term a -> Term a -> Term a
symmetricDifferenceTerm a b = (a `differenceTerm` b) `unionTerm` (b `differenceTerm` a)

