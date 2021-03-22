
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RankNTypes             #-}

module Sky.ProblemCases.TypeVariableEscape where

g :: (forall a. a) -> b
g x = undefined

--f = \x -> g x         -- Error

{- Problem:
Couldn't match expected type `a' with actual type `t' because type variable `a' would escape its scope
      This (rigid, skolem) type variable is bound by ...
-}

-- Fix by specifying the type for x:
f = \(x :: forall a. a) -> g x

-- Or for f in this case:
--f :: (forall a. a) -> b
--f = \x -> g x
