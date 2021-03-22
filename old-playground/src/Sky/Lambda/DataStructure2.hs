
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}  -- Type inference fucks up everything

module Sky.Lambda.DataStructure2 where

----------------------------------------------------------------------------------------------------
-- Product type (aka tuple)

type LProd a b = forall x. (a -> b -> x) -> x

lprod :: a -> b -> LProd a b
-- lprod :: a -> b -> (forall x. (a -> b -> x) -> x)
lprod a b = \op -> op a b

lfst :: LProd a b -> a
-- lfst :: (forall x. (a -> b -> x) -> x) -> a
lfst p = p $ \a b -> a

lsnd :: LProd a b -> b
-- lsnd :: (forall x. (a -> b -> x) -> x) -> b
lsnd p = p $ \a b -> b

----------------------------------------------------------------------------------------------------
-- Sum type (aka either)

type LSum a b = forall x. (a -> x) -> (b -> x) -> x

lleft :: a -> LSum a b
--lleft :: a -> (forall x. (a -> x) -> (b -> x) -> x)
lleft a = \opa opb -> opa a

lright :: b -> LSum a b
--lright :: b -> (forall x. (a -> x) -> (b -> x) -> x)
lright b = \opa opb -> opb b

lgetLeft :: LSum a b -> a
--lgetLeft :: (forall x. (a -> x) -> (b -> x) -> x) -> a
lgetLeft s = s (\a -> a) (\b -> error "No left in LRight") -- Similar to what haskell does

lgetRight :: LSum a b -> b
--lgetRight :: (forall x. (a -> x) -> (b -> x) -> x) -> b
lgetRight s = s (\a -> error "No right in LLeft") (\b -> b) -- Similar to what haskell does

----------------------------------------------------------------------------------------------------
-- Sum takes 2 args, we'd like it to take a tuple

type LNiceSum a b = forall x. (LProd (a -> x) (b -> x)) -> x

-- lNiceLeft :: a -> LNiceSum a b
-- --lNiceLeft :: a -> (forall x. ((a -> y) -> (b -> y) -> x) -> x) -> y
-- lNiceLeft a = \fpair -> (lfst fpair) a

-- lNiceRight :: b -> LNiceSum a b
-- --lNiceRight :: b -> (forall x. ((a -> y) -> (b -> y) -> x) -> x) -> y
-- lNiceRight b = \fpair -> (lsnd fpair) b

toNiceSum :: LSum a b -> LNiceSum a b
-- toNiceSum :: (forall x. (a -> x) -> (b -> x) -> x) ->
--   ( (forall z. ((a -> y) -> (b -> y) -> z) -> z) -> y )
toNiceSum simpleSum = \fpair -> simpleSum (lfst fpair) (lsnd fpair)

----------------------------------------------------------------------------------------------------
-- Isn't this a funny duality between sum and product?

type LFunnyProd a b = forall x. (LSum (a -> x) (b -> x)) -> x

toFunnyProd :: LProd a b -> LFunnyProd a b
toFunnyProd simpleProd = \feither -> simpleProd (\a b -> feither (\f -> f a) (\f -> f b) )

----------------------------------------------------------------------------------------------------
-- Btw void must then be:

-- data Void = Void
type LVoid = forall a. a -> a

lVoid :: LVoid
lVoid = id

----------------------------------------------------------------------------------------------------
-- Recursive data structures

-- data MyList a =
--     Nil
--   | Cons a (MyList a)

-- This can't be typed in Haskell (Cycle in type synonym declarations):
-- type LMyList a = forall x. x -> (a -> LMyList a -> x) -> x

-- So we use a workaround:
newtype LMyList a = WrapMyList (forall x. x -> (a -> LMyList a -> x) -> x)
_unWrap (WrapMyList f) = f

lNil :: LMyList a
lNil = WrapMyList $ \nil cons -> nil

lCons :: a -> LMyList a -> LMyList a
lCons x xs = WrapMyList $ \nil cons -> cons x xs

lHead :: LMyList a -> a
lHead (WrapMyList f) = f (error "Empty list") (\x xs -> x)

lTail :: LMyList a -> LMyList a
lTail (WrapMyList f) = f (error "Empty List") (\x xs -> xs)

-- Want map? :)
lMap :: (a -> b) -> LMyList a -> LMyList b
lMap f (WrapMyList l) = l lNil (\x xs -> lCons (f x) (lMap f xs))

----------------------------------------------------------------------------------------------------
{- The rest doesn't compile with ghc 8 anymore:
    Cannot instantiate unification variable ‘x0’
    with a type involving foralls: LPreList Bool x
    GHC doesn't yet support impredicative polymorphism
-}

{-
----------------------------------------------------------------------------------------------------
-- Combinations

-- This will be useful later on (Do you know Data.Fix?)
-- data PreList a rec =
--     Nil
--   | Cons a rec
type LPreList a rec = LSum LVoid (LProd a rec)

plNil :: LPreList a x
--  plNil = lleft lVoid   -- ARGH: https://mail.haskell.org/pipermail/haskell-cafe/2013-January/105612.html
-- Now i'm just desperately trying to convice the type inference...
-- Rem: lleft a = \opa opb -> opa a
plNil = \opa opb -> opa lVoid       -- Oh, come on.

plCons :: forall a x. a -> LPreList a x -> LPreList a (LPreList a x)
-- plCons x xs = lright $ lprod x xs

-- TYPE INFERENCE Again! I hate this.
-- Rem: lright b = \opa opb -> opb b
-- Rem: lprod a b = \op -> op a b
plCons x xs = \opa opb -> opb (\op -> op x xs)

-- Let's expand LPreList to see what's going on there:
type LPreList' a rec =
    forall x. (LVoid -> x) -> (LProd a rec -> x) -> x
type LPreList'' a rec = forall x.
    ((forall v. v -> v) -> x) ->                -- (LVoid -> x)       --  would be much cooler if it was just x
    ((forall y. (a -> rec -> y) -> y) -> x) ->  -- (LProd a rec -> x) --  i'd like simply (a -> rec -> x) here
    x

-- This is much more sensible (see LMyList above):
type LPreList2 a rec = forall x. x -> (a -> rec -> x) -> x

-- So let's convert this
makePreList2 :: LPreList a rec -> LPreList2 a rec
makePreList2 preList =
    \nil cons -> preList
       (\v -> v nil)    -- LVoid -> x
       (\p -> p cons)   -- LProd a rec -> x

-- huh, that looks awefully like repetition!
-- What is this function here:
--    f :: v -> (v -> r) -> r
--    f value function = function value
-- It's "flip ($)" a.k.a. Data.Function.&

-- The reason is:
-- For a data value v, the idiom "\f -> f v" is very useful.
-- But if that's actually a function and we'd like to apply it, we have to undo that again!
-- (\f -> f v) (\x -> x z) = (\x -> x z) v = v z
-- ($ v) ($ z) = v z

-- Let's just copy (&) from Data.Function:
infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

-- Oh, then we can translate something like this:
data Wrapper a = Wrapper a
-- into
lWrapper :: a -> forall x. (a -> x) -> x
lWrapper v = ($ v)    -- or simply lWrapper = &
-- This is fun :)

-- The type signature of "lWrapper True" :: (Bool -> x) -> x
-- just says "I have a bool, give me something to do with it"

-- Now, back to the composition "LPreList":
makePreList2' :: LPreList a rec -> LPreList2 a rec
makePreList2' preList = \nil cons -> preList ((&) nil) ((&) cons)
-- BTW: i don't quite get why we can't use ($ nil) and ($ cons)

-- So let's go with a concrete example here.
examplePreList :: forall x. LPreList Bool (LPreList Bool (LPreList Bool x))
examplePreList = plCons True $ plCons False $ plNil
-- Oh, the type encodes the length of the list... we'll get to that later.

-}
