
{-# OPTIONS_GHC
    -Wno-missing-signatures
    -Wno-unused-do-bind
    -Wno-type-defaults
#-}
{-# LANGUAGE FlexibleInstances #-}

module Ideas.Old.FunctionMonoid where

-- Need a newtype because we clash with the instance Monoid b => Monoid (a -> b) defined in ‘GHC.Base’
newtype Endo a = Endo (a -> a)

instance Semigroup (Endo a) where
  (<>) (Endo a) (Endo b) = Endo (b . a)  -- Functions that come earlier should be inner

instance Monoid (Endo a) where
  -- mempty :: a -> a
  mempty = Endo id
  -- mappend :: (a -> a) -> (a -> a) -> (a -> a)
  mappend = (<>)

-- Now, since (Endo a) is a monoid and ((,) a) is a Monad if a is a Monoid
-- we can use (Endo a, x) as a monad

-- instance Monoid (Int -> Int) where
--   -- mempty :: a -> a
--   mempty = id
--   -- mappend :: (a -> a) -> (a -> a) -> (a -> a)
--   mappend = flip (.)  -- Could be without the flip

-- instance Functor ((,) a) where
--     fmap f (x,y) = (x, f y)

-- instance Monoid a => Applicative ((,) a) where
--     pure x = (mempty, x)
--     (u, f) <*> (v, x) = (u `mappend` v, f x)

-- This is already defined in GHC.Base now (since 4.9.0.0, May 2016)
-- instance Monoid a => Monad ((,) a) where
--     (u, a) >>= k = case k a of (v, b) -> (u `mappend` v, b)

runXM :: a -> (Endo a, b) -> (a, b)
runXM i (Endo f, v) = (f i, v)

testme = runXM (0 :: Int) $ do
  (Endo (+ 0), 0)  -- Start out with an initial state
  (Endo (+ 1), 5)
  (Endo (+ 1), 123)

-- Ok it works but it isn't very useful
