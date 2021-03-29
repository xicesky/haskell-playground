
module Ideas.ELens where

import Data.Kind (Type, Constraint)
import Data.Typeable hiding (typeOf)
import Type.Reflection
import Data.Profunctor.Indexed

{--------------------------------------------------------------------------------------------------}
{-
Can we get lenses that "act like indices", i.e. they
can be compared using Eq, even if they point to different types?
-}

{- type of Optic A_Lens NoIx s t a b
= Optic A_Lens '[] s t a b
= forall p i. Profunctor p => Optic_ A_Lens p i (Curry '[] i) s t a b
= forall p i. Profunctor p => Optic_ A_Lens p i i s t a b
= forall p i. Profunctor p => Constraints A_Lens p => Optic__ p i i s t a b
= forall p i. Profunctor p => Strong p => Optic__ p i i s t a b
= forall p i. Strong p => Optic__ p i i s t a b
= forall p i. Strong p => p i a b -> p i s t

-}
type OpticLens s t a b = forall p i. Strong p => p i a b -> p i s t

-- For p i a b = a -> b
type OpticLensSetter s t a b = (a -> b) -> (s -> t)

-- For p i a b = a -> r
type OpticLensGetter s t a b r = (a -> r) -> (s -> r)

-- For p = Star f
type OpticLens' s t a b f = (a -> f b) -> (s -> f t)

data ELens :: Type -> Type -> Type -> Type where

-- N.B.: Either (Eq a) or (Eq b) is actually enough
dynEq :: (Eq a, Typeable a, Typeable b) => a -> b -> Bool
dynEq a b = case typeOf a `eqTypeRep` typeOf b of
    Just HRefl  -> a == b
    Nothing     -> False
