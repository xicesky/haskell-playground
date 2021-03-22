
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Sky.Ideas.TypeIso where

-- Can we use type families to define isomorphisms on types?

{- 
    If we read this:
        class (BType (F x)) => AType x where
            type F x :: *
    As a function on types:
        F :: AType -> BType

    Then maybe we can construct isomorphisms out of pairs of such functions?
-}

--class (BType (F x)) => AType x where
--    type F x :: *

---- This doesn't work: Cycle in class declaration
--class (AType (G x)) => BType x where
--    type G x :: *

data A = A
data B = B
data FAB
data FBA

class TFunction f a where
    type ApplyF f a :: *

instance TFunction FAB A where
    type ApplyF FAB A = B

instance TFunction FBA B where
    type ApplyF FBA B = A


class (ApplyI f a ~ b, UnapplyI f b ~ a) => TIso f a b where
    type ApplyI f a :: *
    type UnapplyI f b :: *

---- This won't work
--instance TIso FAB a b where
--    type ApplyI f a = b
--    type UnapplyI f b = a

-- Now you can run this on types:

exampleApplyF :: ApplyF FAB A
exampleApplyF = B
