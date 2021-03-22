
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE TypeFamilies           #-}

module Sky.Ideas.MultiparamElimination where

--  Currying for type functions!?

data Bla x y
    = Bla1 x
    | Bla2 x y
    -- ...

-- Using type families to define "type tuples"
-- https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#closed-type-families

type family TFst tt where
    TFst (a, b) = a

type family TSnd tt where
    TSnd (a, b) = b

data Blubb tt
    = Blubb1 (TFst tt)
    | Blubb2 (TFst tt) (TSnd tt)

-- Proof of isomorphism
bla2blubb :: Bla x y -> Blubb (x,y)
bla2blubb (Bla1 x) = Blubb1 x
bla2blubb (Bla2 x y) = Blubb2 x y

blubb2bla :: Blubb (x,y) -> Bla x y
blubb2bla (Blubb1 x) = Bla1 x
blubb2bla (Blubb2 x y) = Bla2 x y
