
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE BangPatterns           #-}

{- | A witness for arbitrary recursive ADTs that expresses finiteness without
    sacrificing lazyness.
-}

module Sky.Ideas.FiniteRecursiveType where

import Data.Fix

----------------------------------------------------------------------------------------------------

newtype Finite n r = Finite { unFinite :: n r }

instance Show (n r) => Show (Finite n r) where
    show (Finite a) = show a

type FiniteFix f = Fix (Finite f)

{-
Problem: What if someone goes ahead and just does this:
-}

data ListR a r = Nil | Cons a r
    deriving (Show, Eq, Functor)

type List a = Fix (ListR a)
type FiniteList a = FiniteFix (ListR a)

oh_come_on0 :: FiniteList Int
oh_come_on0 = Fix . Finite . Cons 0 $ oh_come_on0

{-
Solution: Export only stuff that allows us to create finite data.
The problem is that "Fix . Finite" is very general any allows us
to create infinite lists.
-}

fix :: f (Fix f) -> Fix f
fix !r = Fix r

fixFinite :: n (Fix (Finite n)) -> Fix (Finite n)
fixFinite !r = fix (Finite r)

oh_come_on1 :: FiniteList Int
oh_come_on1 = fixFinite . Cons 0 $ oh_come_on1

{- That won't work, of course:
    - ListR is still lazy, so oh_come_on1 will run
    - Detection is not at compile time
    - Forcing strict evaluation in fixFinite is just the same as forcing
        strict evaluation in the first place!
-}

{- We need to prohibit the use of arbitrary variables on the right of fixFinite!
    This means we want to enforce a partial order in values of type "FiniteFix a"
    at compile time.
    Since the compile can only make judgements on types, we need to encode the
    order in types.
-}
