
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Sky.Useless.GeneralizedNewtypeDerivingBug where

newtype Age = MkAge { unAge :: Int }

type family Inspect x
type instance Inspect Age = Int
type instance Inspect Int = Bool

class BadIdea a where
  bad :: a -> Inspect a

instance BadIdea Int where
  bad = (> 0)

deriving instance BadIdea Age

{-
src\Sky\Useless\GeneralizedNewtypeDerivingBug.hs:19:1: error:
    * Couldn't match representation of type `Bool' with that of `Int'
        arising from a use of `GHC.Prim.coerce'
    * In the expression:
          GHC.Prim.coerce (bad :: Int -> Inspect Int) :: Age -> Inspect Age
      In an equation for `bad':
          bad
            = GHC.Prim.coerce (bad :: Int -> Inspect Int) :: Age -> Inspect Age
      When typechecking the code for `bad'
        in a derived instance for `BadIdea Age':
        To see the code I am typechecking, use -ddump-deriv
      In the instance declaration for `BadIdea Age'
Failed, modules loaded: none.

-> GeneralizedNewtypeDeriving seems to be fine now :)
-}
