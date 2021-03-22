
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}

module Old.Sky.Compositional.Demo2Parametric where

----------------------------------------------------------------------------------------------------

class Profunctor p where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
    dimap f g = lmap f . rmap g

    lmap :: (a -> b) -> p b c -> p a c
    lmap f = dimap f id

    rmap :: (b -> c) -> p a b -> p a c
    rmap = dimap id

instance Profunctor (->) where
    --dimap :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
    dimap ab cd bc = cd . bc . ab
    lmap ab bc = bc . ab
    rmap bc ab = bc . ab

-- When fixing the first argument, profunctors can be used as functors!
instance Profunctor f => Functor (f a) where
    fmap = rmap

----------------------------------------------------------------------------------------------------
-- Terms from a functor

data Term f = Term (f (Term f))

unTerm :: Term f -> f (Term f)
unTerm (Term x) = x

----------------------------------------------------------------------------------------------------
-- Terms with "holes"

data TermFS f a = TermFS (f (TermFS f a) (TermFS f a)) | PlaceFS a

----------------------------------------------------------------------------------------------------
-- Some intermediate presentation for eval

data IntFn = CInt Int | FInt (IntFn -> IntFn)

appIntFn :: IntFn -> IntFn -> IntFn
appIntFn (CInt _) _ = error "Expected a function"
appIntFn (FInt f) i = f i

instance Show IntFn where
    show (CInt i) = "CInt " ++ show i
    show (FInt _) = "FInt"

----------------------------------------------------------------------------------------------------

data Exp0
    = Lam0 String Exp0
    | Var0 String
    | App0 Exp0 Exp0
    | Let0 String Exp0 Exp0
    | Error0

data Exp1 n e
    = Lam1 n e
    | Var1 n
    | App1 e e
    | Let1 n e e
    | Error1

rename :: (Eq n) => n -> n -> Term (Exp1 n) -> Term (Exp1 n)
rename n0 n1 (Term (Lam1 n e))
    | n == n0   = Term $ Lam1 n e
    | otherwise = Term $ Lam1 n (rename n0 n1 e)
rename n0 n1 (Term (Var1 n))
    | n == n0   = Term $ Var1 n1
    | otherwise = Term $ Var1 n
rename n0 n1 (Term (App1 f x)) = Term $ App1 (rename n0 n1 f) (rename n0 n1 x)
rename n0 n1 (Term (Let1 n a e))
    | n == n0   = error "PROBLEM"
    | otherwise = Term $ Let1 n (rename n0 n1 a) (rename n0 n1 e)

----------------------------------------------------------------------------------------------------

-- But we could just make it a function...

data Exp2 e
    = Lam2 (e -> e)
    | App2 e e
    -- ? | Let e (e -> e)
    | Error2

-- E.g. \x y. x
exp2Lfst :: Term Exp2
exp2Lfst =
    Term $ Lam2 $ \x ->
    Term $ Lam2 $ \y ->
    x

-- An evaluator would be:
eval2 :: Exp2 IntFn -> IntFn
eval2 (Error2)   = error "Encountered error"
eval2 (App2 x y) = appIntFn x y
eval2 (Lam2 f)   = FInt f

-- But this is impossible:
instance Functor Exp2 where
    fmap :: (a -> b) -> Exp2 a -> Exp2 b
    fmap f (App2 x y) = App2 (f x) (f y)
    fmap f (Error2) = Error2
    fmap f (Lam2 g) = Lam2 $ \b -> f (g undefined) -- What to put here? There's no way to make a b into an a

-- And we can't define a catamorphism builder function like this
cata2 :: forall a. (Exp2 a -> a) -> Term Exp2 -> a
cata2 f e = let
    u :: Exp2 (Term Exp2)
    u = unTerm e
    t :: Exp2 a
    t = fmap (cata2 f) u    -- because this won't work
    in f t

----------------------------------------------------------------------------------------------------

-- So we can try to use TermFS instead of term, so we can place arbitrary values in those "holes"
-- And Expressions are really profunctors

data Exp3 a e
    = Lam3 (a -> e)
    | App3 e e
    | Error3

instance Profunctor Exp3 where
    dimap :: (a -> b) -> (c -> d) -> Exp3 b c -> Exp3 a d
    dimap f g (Error3) = Error3
    dimap f g (App3 c0 c1) = App3 (g c0) (g c1)
    dimap f g (Lam3 b_c) = Lam3 (dimap f g b_c) -- Since functions are profunctors

-- And our cata function looks like this:
cata3 :: forall a. (Exp3 a a -> a) -> TermFS Exp3 a -> a
cata3 f (TermFS uu) = let
    u :: Exp3 (TermFS Exp3 a) (TermFS Exp3 a)
    u = uu
    t :: Exp3 a a
    t = dimap PlaceFS (cata3 f) u   -- PlaceFS takes an "a" to make a "TermFS Exp3 a"
    in f t
cata3 f (PlaceFS x) = x             -- If we encounter one of the terms we replaced, we know they are already evaluated

{- For reference:
dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
data TermFS f a = TermFS (f (TermFS f a) (TermFS f a)) | PlaceFS a
PlaceFS :: a -> TermFS f a
-}

-- E.g. \x y. x
exp3Lfst :: TermFS Exp3 a
exp3Lfst =
    TermFS $ Lam3 $ \x ->
    TermFS $ Lam3 $ \y ->
    x

-- An evaluator would be:
eval3 :: Exp3 IntFn IntFn -> IntFn
eval3 (Error3)   = error "Encountered error"
eval3 (App3 x y) = appIntFn x y
eval3 (Lam3 f)   = FInt f

exp3Demo :: IntFn
exp3Demo = cata3 eval3 exp3Lfst `appIntFn` (CInt 1) `appIntFn` (CInt 2)

-- But we can encode bad terms:
exp3Bad :: TermFS Exp3 a
exp3Bad =
    TermFS $ Lam3 $ \x -> case x of
        TermFS (App3 _ _)   -> TermFS (App3 x x)    -- This is wrong!
        _                   -> x

{- i.e.:
Our terms can recognize stuff in those lambda variables!
We'd like them to just be using them "plainly".
We'd like our terms to have type "forall a. Exp3 a":
-}

data Term3 f = Term3 (forall a. Term3In f a)
data Term3In f a = In3 (f a (Term3In f a))

exp3Lfst2 :: Term3 Exp3
exp3Lfst2 = Term3 $
    In3 $ Lam3 $ \x ->
    In3 $ Lam3 $ \y ->
    undefined   -- We can't put x here anymore, because it could have any type: "forall a. a"

----------------------------------------------------------------------------------------------------
-- So let's encapsulate variables as terms:

data Exp4 v e
    = Var4 v
    | Lam4 (v -> e)
    | App4 e e
    | Error4

instance Profunctor Exp4 where
    dimap :: (a -> b) -> (c -> d) -> Exp4 b c -> Exp4 a d
    dimap f g (Error4) = Error4
    dimap f g (App4 c0 c1) = App4 (g c0) (g c1)
    dimap f g (Lam4 b_c) = Lam4 (dimap f g b_c) -- Since functions are profunctors
    dimap f g (Var4 v) = Var4 (undefined v) -- Our variables have type b, but we can't map that...

-- So variables have to be part of the "Term" structure, like PlaceFS in TermFS

----------------------------------------------------------------------------------------------------

data Term5 f = Term5 (forall v. Term5In f v)
data Term5In f v = In5 (f v (Term5In f v)) | Var5 v

data Exp5 v e
    = Lam5 (v -> e)
    | App5 e e
    | Error5

instance Profunctor Exp5 where
    dimap :: (a -> b) -> (c -> d) -> Exp5 b c -> Exp5 a d
    dimap f g (Error5) = Error5
    dimap f g (App5 c0 c1) = App5 (g c0) (g c1)
    dimap f g (Lam5 b_c) = Lam5 (dimap f g b_c) -- Since functions are profunctors

-- And our cata function looks like this:
cata5 :: forall a. (Exp5 a a -> a) -> Term5 Exp5 -> a
cata5 f (Term5 t5) = cat t5 where   -- Here we force type type of variables "v" to be "a"
    cat :: Term5In Exp5 a -> a
    cat (In5 u) = let               -- u :: Exp5 a (Term5In Exp5 a)
        t :: Exp5 a a
        t = dimap id cat u
        in f t
    cat (Var5 x) = x                -- Variables are already of type a

exp5Lfst :: Term5 Exp5
exp5Lfst = Term5 $
    In5 $ Lam5 $ \x ->
    In5 $ Lam5 $ \y ->
    Var5 x

evalAlg5 :: Exp5 IntFn IntFn -> IntFn
evalAlg5 (Error5)   = error "Encountered error"
evalAlg5 (App5 x y) = appIntFn x y
evalAlg5 (Lam5 f)   = FInt f

eval5 :: Term5 Exp5 -> IntFn
eval5 = cata5 evalAlg5

eval5Demo :: IntFn
eval5Demo = eval5 exp5Lfst `appIntFn` (CInt 1) `appIntFn` (CInt 2)

-- Bad terms can be encoded as Term5In, but not as Term5:
exp5InBad :: Term5In Exp5 (Term5In Exp5 t)
exp5InBad =
    In5 $ Lam5 $ \x -> case x of
        In5 (App5 _ _)  -> In5 (App5 (Var5 x) (Var5 x))     -- This is wrong!
        _               -> Var5 x

exp5Bad :: Term5 Exp5
exp5Bad = Term5 (undefined exp5InBad)   -- Couldn't match type `v' with `Term5In Exp5 t0'
                --  ^-- can't convert: Term5In Exp5 t -> (forall a. a)

----------------------------------------------------------------------------------------------------
-- Additional idea: Can we make "let x = ... x ... in x" part of Term?

-- This would represent "DAG-like" structures over a functor f
-- Can we get something like "Eq" for variables?

data Term6 f = Term6 (forall v. Term6In f v)
data Term6In f v = In6 (f (Term6In f v)) | Var6 v | LetRec6 (v -> Term6In f v)

data Exp6 e
    = App6 e e
    | Error6

instance Functor Exp6 where
    fmap :: (a -> b) -> Exp6 a -> Exp6 b
    fmap f (App6 x y) = App6 (f x) (f y)
    fmap f (Error6) = Error6

-- And our cata function looks like this:
cata6 :: forall a. (Exp6 a -> a) -> Term6 Exp6 -> a
cata6 f (Term6 t6) = cat t6 where   -- Here we force type type of variables "v" to be "a"
    cat :: Term6In Exp6 a -> a
    cat (In6 u) = let               -- u :: Exp6 a (Term6In Exp6 a)
        t :: Exp6 a
        t = fmap cat u
        in f t
    cat (LetRec6 r) = let
        aR :: a
        aR = cat (r aR)
        in aR
    cat (Var6 x) = x                -- Variables are already of type a

-- But it doesn't help us to define lambdas... which might be ok for a DAG
let6 :: (Term6In Exp6 v -> Term6In Exp6 v) -> Term6In Exp6 v
let6 f = LetRec6 (f . Var6)

-- And Eq needs unique names
instance (Eq v, Eq (f (Term6In f v))) => Eq (Term6In f v) where
    (In6 e0) == (In6 e1) = e0 == e1
    (Var6 n0) == (Var6 n1) = n0 == n1
    (LetRec6 r0) == (LetRec6 r1) = r0 newV == r1 newV where
        newV = undefined
