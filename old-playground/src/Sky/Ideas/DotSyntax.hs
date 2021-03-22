
module Sky.Ideas.DotSyntax where

import Prelude hiding ((.))
import Control.Category
import Data.Function hiding ((.))
import Control.Monad
import Control.Arrow (Kleisli(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Dual(..), Endo(..))

-- Just a few example defs
data A = A deriving (Show)
data B = B deriving (Show)
data C = C deriving (Show)
data D = D deriving (Show)
data E = E deriving (Show)

fAB :: A -> B
fAB a = B
fBC :: B -> C
fBC b = C
fCD :: C -> D
fCD c = D
fDE :: D -> E
fDE d = E

infixl 0 &:
(&:) :: (a -> b) -> (b -> c) -> (a -> c)
(&:) f g = g . f

-- Often we'd use "point-free style" to turn this:

example0 :: A -> D
example0 a = fCD (fBC (fAB a))

-- into this

example1 :: A -> D
example1 = fCD . fBC . fAB

-- From a imperative standpoint, you could see that as a "series of actions"

example2 :: A -> D
example2
    =   fAB
    &:  fBC
    &:  fCD

-- This works the same for monadic function composition with <=< and >=>

example3 :: (Monad m) => A -> m D
example3
    =   (return . fAB)  -- A -> m B
    >=> (return . fBC)  -- B -> m C
    >=> (return . fCD)  -- C -> m D

-- and
example3' :: A -> D
example3' = runIdentity . example3

-- Isn't this a bit like "do" for categories?

example4 :: A -> D
example4
    =   fAB
    >>> fBC
    >>> fCD

kr :: Monad m => (a -> b) -> Kleisli m a b
kr = Kleisli . (return .)

example5 :: (Monad m) => A -> m D
example5 = runKleisli
    $   kr fAB      -- A -> m B
    >>> kr fBC      -- B -> m C
    >>> kr fCD      -- C -> m D

-- If all the types would be the same, we could use the Monoid Endo

fAA :: A -> A
fAA a = A

type Prog a = Dual (Endo a)
prog :: (a -> a) -> Prog a
prog = Dual . Endo
unProg :: Prog a -> (a -> a)
unProg = appEndo . getDual

example6 :: A -> A
example6 = unProg $ mconcat fns where
    fns :: [Prog A]
    fns =
        [ prog fAA
        , prog fAA
        , prog fAA
        ]

-- And since "Monoid a => Monad ((,) a)"
doF :: (a -> a) -> (Prog a, ())
doF f = (prog f, ())

example7 :: A -> A
example7 = unProg $ fst $ fns where
    fns :: (Prog A, ())
    fns = do
        doF fAA
        doF fAA
        doF fAA

{- So, to use the "Sequential" part of the do Syntax, we'd have
    to use Data.Dynamic (or similar), which would defeat our type system...
-}
