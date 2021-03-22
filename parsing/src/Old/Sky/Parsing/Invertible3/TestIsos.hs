
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Old.Sky.Parsing.Invertible3.TestIsos where

import Prelude hiding (id, (.))
import Control.Category (Category(..))

import Old.Sky.Parsing.Invertible3.Isomorphism
import Old.Sky.Parsing.Invertible3.TH
import Data.Data (Data)

-- Temp for GHCI
import Language.Haskell.TH

data Example
    = Example1
    | Example2 Int
    | Example3 Int Int
    | Example4 { a :: Int, b :: Bool, c :: String }
    | Int :~: Int
    --  ...
    deriving (Show, Data)

-- Method A: Create all isomorphisms using a namer
makeIsomorphisms ''Example defaultIsoNamer

-- Method B: Make a single isomorphism with a given name
makeIsomorphism "example1'" [|Example1|]

-- Method C: Manually define isomorphisms, using TH for the impl only
exampleM1 :: Iso () Example
exampleM1 = $(isoConstr 'Example1)

exampleM2 :: Iso Int Example
exampleM2 = $(isoConstr 'Example2)

exampleM3 :: Iso (Int, Int) Example
exampleM3 = $(isoConstr 'Example3)

exampleM4 :: Iso (Int, Bool, String) Example
exampleM4 = $(isoConstr 'Example4)

exampleM5 :: Iso (Int, Int) Example
exampleM5 = $(isoConstr '(:~:))

eval :: a -> String
eval !a = "Ok."

-- Run e.g.:
-- ghci -ddump-splices -XDeriveDataTypeable -XTemplateHaskell -isrc src/Sky/Parsing/Invertible3/TestIsos.hs
