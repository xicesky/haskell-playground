
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

module Old.Sky.Parsing.LR where

import Old.Sky.Util.GraphMonad

import Data.Functor.Identity (Identity)

import Prelude hiding (lookup, (!!))
import Old.Sky.Util.NewContainer

{- Example Grammar
E' -> E
E -> E + T | T
T -> T * F | F
F -> ( E ) | id
-}

data GrammarItem n t
    = Terminal t
    | Nonterminal n
    deriving (Show, Eq)

type GrammarRH n t = [ [ GrammarItem n t ] ]
type Grammar n t = (n, HashMap n (GrammarRH n t))

exampleG :: Grammar String Char
exampleG = ("E", fromList $
    [ ( "E",    [ [ Nonterminal "E", Terminal '+', Nonterminal "T" ]
                , [ Nonterminal "T" ]
                ] )
    , ( "T",    [ [ Nonterminal "T", Terminal '*', Nonterminal "F" ]
                , [ Nonterminal "F" ]
                ] )
    , ( "F",    [ [ Terminal '(', Nonterminal "E", Terminal ')' ]
                , [ Terminal 'i' ]
                ] )
    ])

-- traverseGraph :: forall m n t. (Monoid m) => k -> HashMap k (GrammarRH k t) -> (k -> GrammarRH k t -> m) -> m
-- traverseGraph start map step = doTraverse empty start mzero where
--     doTraverse :: HashSet n -> n -> m -> m
--     doTraverse visited name m = if name `isMemberOf` visited then m else doStep
--     doStep :: GrammarItem -> _
--     doStep (Nonterminal m) = if m `isMemberOf` visited then (enqueue n `mappend`) else id $
--         step 
