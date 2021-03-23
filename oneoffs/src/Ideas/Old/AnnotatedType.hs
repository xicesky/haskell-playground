
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE UndecidableInstances   #-}

{- Tagged dynamic type universes
-}
module Ideas.Old.AnnotatedType where

-- Take some implementation of types and values
data BaseType r
    = BaseType Integer
    deriving (Show, Eq)

data BaseValue r
    = Value Integer
    deriving (Show)

data TypeError = TypeError String
    deriving (Show)

-- Introducing function types
data OurType r
    = FunctionType r r
    | SimpleType (BaseType r)
    deriving (Show, Eq)

data OurValue r
    = FunctionValue (r -> r)
    | SimpleValue (BaseValue r)

instance Show (OurValue r) where
    show (SimpleValue v) = "SimpleValue " ++ show v
    show (FunctionValue _) = "FunctionValue"

-- Fixpoint
data Fix f = Fix (f (Fix f))

instance Show (f (Fix f)) => Show (Fix f) where
    show (Fix x) = "(" ++ show x ++ ")"
  
instance Eq (f (Fix f)) => Eq (Fix f) where
    (Fix a) == (Fix b) = a == b

type Typ = Fix OurType
type Val = Fix OurValue

-- We will handle typed values as
type TypedValue = (Typ, Val)
type SafeValue = Either TypeError TypedValue

----------------------------------------------------------------------------------------------------
-- Quickly "lift" Haskell stuff to val

class Lift a where
    liftV :: a -> Val

instance Lift Val where
    liftV = id

instance Lift c => Lift (Val -> c) where
    liftV f = Fix $ FunctionValue $ f' where
        f' = liftV . f

instance Lift Integer where
    liftV = Fix . SimpleValue . Value

----------------------------------------------------------------------------------------------------

-- Application which might crash if used wrongly
unsafeApp :: Val -> Val -> Val
unsafeApp (Fix (FunctionValue f)) v = f v
-- anything else will crash due to missing patterns
unsafeApp _ _ = error "unsafeApp is unsafe."


-- Application
safeApp :: SafeValue -> SafeValue -> SafeValue
safeApp (Left x) _ = Left x
safeApp _ (Left x) = Left x
safeApp (Right f) (Right a) = app f a where
    app :: TypedValue -> TypedValue -> Either TypeError TypedValue
    app (ft@(Fix (FunctionType d c)), v0) (t1, v1)
        | d == t1       = Right $ (c, unsafeApp v0 v1)
        | otherwise     = Left $ TypeError $ "Type error: (Wrong argument type) Cannot apply " ++ show ft ++ " to " ++ show t1
    app (t0, _) (t1, _) = Left $ TypeError $ "Type error: (Not a function)      Cannot apply " ++ show t0 ++ " to " ++ show t1

----------------------------------------------------------------------------------------------------
-- Examples, demo

v1 :: Val
v1 = liftV (1 :: Integer)

vAdd :: Val
vAdd = liftV add where
    add :: Val -> Val -> Val
    add (Fix (SimpleValue (Value a))) (Fix (SimpleValue (Value b))) = liftV (a + b)
    add _ _ = error "Invalid call to vAdd"

unsafeEx0 :: Val
unsafeEx0 = (vAdd `unsafeApp` v1) `unsafeApp` v1

unsafeEx1 :: Val
unsafeEx1 = v1 `unsafeApp` v1

tInt :: Typ
tInt = Fix $ SimpleType $ BaseType 0

tF :: Typ -> Typ -> Typ
tF d c = Fix $ FunctionType d c

tv1 :: SafeValue
tv1 = Right (tInt, v1)

tvAdd :: SafeValue
tvAdd = Right (tInt `tF` (tInt `tF` tInt), vAdd)

safeEx0 :: SafeValue
safeEx0 = (tvAdd `safeApp` tv1) `safeApp` tv1

safeEx1 :: SafeValue
safeEx1 = tv1 `safeApp` tv1

