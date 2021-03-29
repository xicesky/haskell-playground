
{-# LANGUAGE UndecidableSuperClasses #-}
{-
Differentiable data structures
    http://www.cs.nott.ac.uk/~psztxa/publ/jpartial.pdf
    http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.176.2720&rep=rep1&type=pdf
    https://github.com/eelco/gdiff

Also inspired by
    https://hackage.haskell.org/package/recursion-schemes-5.2.2.1/docs/Data-Functor-Foldable.html

There is an implementation in:
    https://michaeldadams.org/papers/scrap_your_zippers/
that uses generic data types ala SYB (which require some casts...)

-}
module Ideas.DiffDS where

import Data.Kind (Type)
import Data.Void
import Data.Fix
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Foldable

{--------------------------------------------------------------------------------------------------}

data VoidF a                            -- 0
-- Const, Identity as usual

data    (a :+: b) x = InL (a x) | InR (b x) -- Either (a x) (b x)
newtype (a :*: b) x = Prod (a x, b x)

newtype P0 a b = P0 a
newtype P1 a b = P1 b
newtype Weaken f a b = Weaken (f a)     -- ↑F   (Weakening)
newtype Comp f g x = Comp (f x (g x))   -- F[G] (Composition)

-- For binary containers g0 g1
newtype Comb2 f g0 g1 x y = Comb (f (g0 x y) (g1 x y))

{--------------------------------------------------------------------------------------------------}

type family Deriv (f :: Type -> Type) :: Type -> Type

--type instance Deriv VoidF = VoidF
type instance Deriv (Const a) = Const Void
type instance Deriv Identity = Const ()

type instance Deriv (a :+: b) = Deriv a :+: Deriv b
type instance Deriv (a :*: b) = (Deriv a :*: b) :+: (a :*: Deriv b)

type instance Deriv (Comp f g) 
    =   Comp (Deriv_0 f) g
    :+: (Comp (Deriv_1 f) g :*: Deriv g)

-- Derviatives for binary (n-ary) containers
type family Deriv_0 (f :: Type -> Type -> Type) :: Type -> Type -> Type
type family Deriv_1 (f :: Type -> Type -> Type) :: Type -> Type -> Type

{-
There is just no way to define
    Deriv_j (F G_0 G_1 ... G_n) = Sum (0 <= i < n) (Deriv_i F) G_0 ... :*: (Deriv_j G_i)
-}

-- For binary containers F (G_0) (G_1)
-- We can't even lift :*: over binary containers :(

-- type instance Deriv_0 (Comb2 f g0 g1) =
--         (Comb2 (Deriv_0 f) g0 g1 :*: Deriv_0 g0)
--     :+: (Comb2 (Deriv_1 f) g0 g1 :*: Deriv_0 g1)


{--------------------------------------------------------------------------------------------------}

type family Path (f :: Type -> Type) :: Type

class Functor f => Differentiable f where
    assemble :: Deriv f a -> a -> f a


-- sup :: forall a b w (e :: a). a -> (b e -> w a b) -> w a b
-- sup = _

-- -- Example
-- data MyTree                             -- t =
--     = Leaf                              -- 1
--     | Node MyTree MyTree                -- + t * t

-- data MyTreeF r                          -- t_b(r) =
--     = FLeaf                             -- 1
--     | FNode r r                         -- + r * r

-- type instance Base (MyTree a) = MyTreeF a

-- data MyTreePath
--     = PHere
--     | PLeft MyTreePath
--     | PRight MyTreePath

-- data MyTreeDeriv                        -- t' =
--     =                                   -- 0

