
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

-- | Simple lenses (from SPJ's talk about lenses) - playground

module Sky.Lens.SimpleLens where

import Data.Functor.Identity
import Control.Applicative (Const(Const,getConst))

data LensR s a = LensR { getR :: s -> a, setR :: a -> s -> s }
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- From Lens' to LensR

set :: forall s a. Lens' s a -> a -> s -> s
set lens v = runIdentity . lens (Identity . const v)

get :: Lens' s a -> s -> a
get lens = getConst . lens Const

lensToLensR :: Lens' s a -> LensR s a
lensToLensR lens = LensR (get lens) (set lens)

-- From LensR to Lens'

lensRToLens :: LensR s a -> Lens' s a
lensRToLens (LensR getter setter) m s = fmap (flip setter s) (m $ getter s)

-- lensRToLens :: forall f s a. Functor f => LensR s a -> (a -> f a) -> s -> f s
-- lensRtoLens = let
--     -- m :: a -> f a
--     -- s :: s
--     -- getter :: s -> a
--     -- setter :: a -> s -> s
--     modded :: f a
--     modded = m (getter s)
--     setA :: a -> s
--     setA = flip setter s
--     in fmap setA modded -- :: f s

-- Lens' utilities

over :: Lens' s a -> (a -> a) -> s -> s
over lens m = runIdentity . lens (Identity . m)

-- modify, returning the old state
overRet :: Lens' s a -> (a -> a) -> s -> (a, s)
overRet lens m =
  -- via: f a = ((,) a)
  lens $ \a -> (a, m a)


-- Example

data Person = Person { _name :: String, _salary :: Int }
    deriving (Show, Eq)

name :: Lens' Person String
name m p = fmap (\x -> p { _name = x }) (m $ _name p)

personX :: Person
personX = Person "Hans Wurst" 10
-- name :: forall f. Functor f => (String -> f String) -> Person -> f Person
-- name m p = let
--     -- m :: String -> f String
--     -- p :: Person
--     modded :: f String
--     modded = m (_name p)
--     setName :: String -> Person
--     setName x = p { _name = x }
--     in fmap setName modded

data A = A ()
data B = B ()
data C = C ()
data D = D ()
data X = X ()

f :: A -> B
f (A ()) = B ()

