
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Sky.Util.NewGraphLike
    (
    ) where

import Prelude hiding (lookup, (!!))
import Sky.Util.NewContainer

class (HasLookup c) => GraphLike c where
    --type RefNameT c :: *
    --type RefNameT c = KeyT c

    --makeReference :: RefNameT c -> ValueT c
    makeReference :: KeyT c -> ValueT c

data Expr r
    = Literal Int
    | Add (Expr r) (Expr r)
    | Mul (Expr r) (Expr r)
    | Reference r
    deriving (Show, Eq, Ord)

type ExprGraph r = HashMap r (Expr r)

instance (Ord r, Hashable r) => GraphLike (ExprGraph r) where
    makeReference :: r -> Expr r
    makeReference = Reference

eval :: (Ord r, Hashable r) => ExprGraph r -> Expr r -> Int
eval m (Literal i) = i
eval m (Add x y) = eval m x + eval m y
eval m (Mul x y) = eval m x * eval m y
eval m (Reference r) = eval m (m !! r)

demoExpr :: ExprGraph String 
demoExpr = fromList
    [ ("a", Literal 5)
    , ("b", Add (Reference "a") (Literal 1))
    , ("main", Mul (Reference "a") (Reference "b"))
    ]

demo = eval demoExpr (Reference "main")
