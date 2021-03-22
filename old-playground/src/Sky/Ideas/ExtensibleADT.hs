
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}

-- Variable `f' occurs more often in the constraint `Data (f (Fix f))' than in the instance head
{-# LANGUAGE UndecidableInstances   #-}

-- Class instances at the bottom use type families
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE KindSignatures         #-}

----------------------------------------------------------------------------------------------------

module Sky.Ideas.ExtensibleADT where

import GHC.Generics
import Data.Data

----------------------------------------------------------------------------------------------------

{-  Sometimes we run into a problem with closed ADTs. This happens a lot e.g. when
    transforming expression trees ("AST"s) where you still want to keep your types
    really tight.
    For example, take a straight forward lambda expression tree like:
-}

data L1Expression n
    = L1Variable n
    | L1Lambda n (L1Expression n)
    | L1Application (L1Expression n) (L1Expression n)
    deriving (Eq, Show)

{-  Let's say the parser also adds some debug info and we'd like to extend this ADT
    to the following ADT:
-}

type DebugInfo = String

data L2Expression n
    = L2Variable n
    | L2Lambda [n] (L2Expression n)
    | L2Application (L2Expression n) (L2Expression n)
    | L2DebugInfo DebugInfo (L2Expression n)
    -- LambdaRef r       -- Reference 
    deriving (Eq, Show)

{-  What exactly transforms L1Expression into something like L2Expression?
    1. We added another alternative "L2DebugInfo"
    2. We had to change all the recursive data types from "L1Expression" to "L2Expression"

    So if we wanted to express L2Expression in terms of L1Expression:
    1. We can easily add another case using a new datatype or "Either":
        data L2Expression n
            = L1toL2 (L1Expression n)
            | L2DebugInfo DebugInfo (L2Expression n)
    2. But the recursive type don't match now. So we need something like Data.Fix

-}

----------------------------------------------------------------------------------------------------

newtype Fix f = Fix { unFix :: f (Fix f) } deriving (Generic, Typeable)
deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)

instance Show (f (Fix f)) => Show (Fix f) where
    show x = "(" ++ show (unFix x) ++ ")"
  
instance Eq (f (Fix f)) => Eq (Fix f) where
    a == b = unFix a == unFix b
  
instance Ord (f (Fix f)) => Ord (Fix f) where
    a `compare` b = unFix a `compare` unFix b

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f

----------------------------------------------------------------------------------------------------

{-  So let's try this then:
-}

data X1Expression n e
    = X1Variable n
    | X1Lambda n e
    | X1Application e e
    deriving (Eq, Show, Functor)

{-
instance Functor (X1Expression n) where
    fmap :: (a -> b) -> X1Expression n a -> X1Expression n b
    fmap f (X1Variable n) = X1Variable n
    fmap f (X1Lambda n e) = X1Lambda n (f e)
    fmap f (X1Application e1 e2) = X1Application (f e1) (f e2)
-}

type E1Expression n = Fix (X1Expression n)

data X2Expression n e
    = X1toX2 (X1Expression n e)
    | X2DebugInfo DebugInfo e
    deriving (Eq, Functor)

instance (Show n, Show e) => Show (X2Expression n e) where
    show (X1toX2 e) = show e
    show (X2DebugInfo d e) = "(DebugInfo " ++ show d ++ " " ++ show e ++ ")"

type E2Expression n = Fix (X2Expression n)

----------------------------------------------------------------------------------------------------

class ExtendedADT a where
    type BaseADT a :: *
    encapsulate :: BaseADT a -> a

-- class ExtendedRecursiveADT a where
--     forget :: a -> BaseADT

instance ExtendedADT (X2Expression n e) where
    type BaseADT (X2Expression n e) = X1Expression n e
    encapsulate = X1toX2
    -- forget (X1toX2 a) = a
    -- forget (X2DebugInfo _ a) = forget a

instance ExtendedADT (E2Expression n) where
    type BaseADT (E2Expression n) = E1Expression n
    encapsulate :: E1Expression n -> E2Expression n
    encapsulate = cata (Fix . X1toX2)

----------------------------------------------------------------------------------------------------
-- Even more general

newtype TPlus t1 t2 r = TPlus { unTPlus :: Either (t1 r) (t2 r) }

instance (Show (t1 r), Show (t2 r)) => Show (TPlus t1 t2 r) where
    show (TPlus (Left t1)) = show t1
    show (TPlus (Right t2)) = show t2

class Embeddable t tsub where
    embed :: tsub -> t

instance Embeddable (TPlus t1 any r) (t1 r) where
    embed = TPlus . Left

instance Embeddable (TPlus any t2 r) (t2 r) where
    embed = TPlus . Right

instance Functor t1 => Embeddable (Fix (TPlus t1 any)) (Fix t1) where
    embed :: Fix t1 -> Fix (TPlus t1 any)
    embed = cata (Fix . embed)

instance Functor t2 => Embeddable (Fix (TPlus any t2)) (Fix t2) where
    embed :: Fix t2 -> Fix (TPlus any t2)
    embed = cata (Fix . embed)

----------------------------------------------------------------------------------------------------

data AttachedDebugInfo r = AttachedDebugInfo DebugInfo r
    deriving (Eq, Show, Functor)

type X3Expression n r = TPlus (X1Expression n) (AttachedDebugInfo) r
type E3Expression n = Fix (TPlus (X1Expression n) (AttachedDebugInfo))

-- Not done yet: We'd like to do this in general:

-- attachDebugInfo :: DebugInfo -> r -> AttachedDebugInfo r
-- attachDebugInfo di = AttachedDebugInfo di

attachDebugInfo :: DebugInfo -> E3Expression n -> E3Expression n
attachDebugInfo di = Fix . TPlus . Right . AttachedDebugInfo di

----------------------------------------------------------------------------------------------------
-- Examples for playing around

x1example1 :: X1Expression String a
x1example1 = X1Variable "a"

x1example2 :: X1Expression String (X1Expression String a)
x1example2 = X1Application (X1Variable "a") (X1Variable "b")

e1example1 :: E1Expression String
e1example1 = (Fix . X1Variable) "a"

e1example2 :: E1Expression String
e1example2 = Fix $ X1Application ((Fix . X1Variable) "a") ((Fix . X1Variable) "b")

----------------------------------------------------------------------------------------------------

{- Of course, someone already did this:
    http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.6303&rep=rep1&type=pdf
    http://bahr.io/pubs/files/bahr11wgp-slides%20(full).pdf
And https://hackage.haskell.org/package/compdata
-}
