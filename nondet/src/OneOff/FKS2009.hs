
{-# OPTIONS_GHC
    -Wno-name-shadowing
#-}

-- 2009 Fischer, Kiselyov, Shan - Purely functional lazy non-deterministic programming
-- https://hackage.haskell.org/package/explicit-sharing
module OneOff.FKS2009 where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Class

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

{-
Required laws for MonadPlus to be valid:

    (M1)    return x >>= k  =  k x
    (M2)      a >>= return  =  a
    (M3) (a >>= k1) >>= k2  =  a >>= \x k1 x >>= k2
    (P1)       empty >>= k  =  empty
    (P2)   (a <|> b) >>= k  =  (a >>= k) <|> (b >>= k)

The (M*) laws are required by 'Monad'. The laws (P1) is required by
'MonadPlus', but (P2) is not required by the base library. It holds
for @MonadPlus []@, but - for example - not for @MonadPlus Maybe@, because:

    (return False <|> return True) >>= guard              = empty
    (return False >>= guard) <|> (return True >>= guard)  = return ()

The laws (A*) (below) is given by 'Alternative', but not required for our
implementation (FIXME really?).

    (A1)       empty <|> a  =  a
    (A2)       a <|> empty  =  a

We will use incomplete pattern matches sometimes, so we also expect
    
    (F1)             fail _ = empty

-}

{-----------------------------------------------------------------------------}
-- Simple things

coin :: MonadPlus m => m Int
coin = return 0 <|> return 1

coin' :: MonadPlus m => m Int
coin' = do
    x <- coin
    y <- coin
    guard (x+y > 0)
    return x

{-----------------------------------------------------------------------------}
-- Permutations

perm :: MonadPlus m => [a] -> m [a]
perm [] = return []
perm (x:xs) = do
    ys <- perm xs
    insert x ys

insert :: MonadPlus m => a -> [a] -> m [a]
insert x [] = return [x]
insert x (y:ys)
    = return (x : y : ys)
    <|> ( (y :) <$> insert x ys)

{-----------------------------------------------------------------------------}
-- Permutation sort

isSorted1 :: [Int] -> Bool
isSorted1 []         = True
isSorted1 [_]        = True
isSorted1 (x:y:ys)   = (x <= y) && isSorted1 (y:ys)

sortInefficient :: MonadPlus m => [Int] -> m [Int]
sortInefficient xs = do
    ys <- perm xs
    guard $ isSorted1 ys
    return ys

-- Try: sortInefficient [1..9] :: [[Int]]

{-----------------------------------------------------------------------------}
-- "Monadic" lists
-- can store nondeterministic values

type MonadND m = (MonadPlus m, MonadFail m)

data List m a
    = Nil
    | Cons (m a) (m (List m a))

-- ugh. smart constructors
nil :: Monad m => m (List m a)
nil = return Nil

cons :: Monad m => m a -> m (List m a) -> m (List m a)
cons x y = return $ Cons x y

toMList :: Monad m => [a] -> m (List m a)
toMList = foldr (cons . return) nil

fromMList :: Monad m => m (List m a) -> m [a]
fromMList ml = ml >>= \case
    Nil -> return []
    Cons mx mxs -> do
        x <- mx
        xs <- fromMList mxs
        return $ x : xs

-- Welcome to bind/case hell
isSorted2 :: MonadPlus m => m (List m Int) -> m Bool
isSorted2 ml = ml >>= \case
    Nil -> return True
    Cons mx mxs -> mxs >>= \case
        Nil -> return True
        Cons my mys -> do
            x <- mx
            y <- my
            if x <= y
            then isSorted2 (cons (return y) mys)
            else return False

{- For comparison:
perm :: MonadPlus m => [a] -> m [a]
perm [] = return []
perm (x:xs) = do
    ys <- perm xs
    insert x ys
-}
perm2 :: MonadND m => m (List m a) -> m (List m a)
perm2 ml = ml >>= \case
    Nil -> nil
    Cons mx mxs -> insert2 mx (perm2 mxs)

{- For comparison:
insert :: MonadPlus m => a -> [a] -> m [a]
insert x [] = return [x]
insert x (y:ys)
    = return (x : y : ys)
    <|> ( (y :) <$> insert x ys)
-}
insert2 :: MonadND m => m a -> m (List m a) -> m (List m a)
insert2 mx ml = mx `cons` ml <|> do
    Cons my mys <- ml       -- MonadFail use, req. fail = mzero
    my `cons` insert2 mx mys

sort2 :: MonadND m => m (List m Int) -> m (List m Int)
sort2 xs = let ys = perm2 xs in do
    True <- isSorted2 ys
    ys
-- sort2 xs = do
--     ys <- fromMList $ perm2 xs  -- Eager evaluation!
--     True <- isSorted2 (toMList ys)
--     toMList ys



-- ... and we fail! (expectedly...)

sort2' :: [Int] -> [[Int]]
sort2' = fromMList . sort2 . toMList

{-----------------------------------------------------------------------------}
-- Explicit sharing = pruning

{- Sharing laws

(SChoice)                share (a <|> b)  =  share a <|> share b
(SFail)                      share empty  =  return empty
(SIgnore)              share a >>= \x. b  =  b                 -- if x free in b
(SFlat)            share (share e >>= k)  =  share e >>= share . k
(SRepeat)  share a >>= \x. k x (share x)  =  share a >>= \x. k x (return x)
(SRdistr)   share a >>= \x.(f x <|> g x)  =  (share a >>= f) <|> (share a >>= g)
(SHNF)      share (return (c x1 ... xn))  =
          share x1 >>= λy1. ... share xn >>= λyn. return (return (c y1 ... yn))
-}

-- Needed?
class Nondet n where
    failure :: n
    (?) :: n -> n -> n

instance Ord a => Nondet (Set a) where
    failure = Set.empty
    (?) = Set.union

{-  This version doesn't satisfy the (SFail) and (SIgnore) laws,
    because it is eager.
-}
share0 :: Monad m => m a -> m (m a)
share0 a = a >>= \x -> return (return x)

-- For demo only memoizes a single, specific type (List Memo Int)
-- FIXME use State monad
newtype Memo a = Memo 
    { unMemo :: [Thunk (List Memo Int)] -> [(a, [Thunk (List Memo Int)])]
    }

data Thunk a = Uneval (Memo a) | Eval a

instance Functor Memo where
    fmap = liftM

instance Applicative Memo where
    pure = return
    (<*>) = ap

instance Monad Memo where
    return x = Memo (\ts -> [(x,ts)])
    m >>= f = Memo (concatMap (\(x,ts) -> unMemo (f x) ts) . unMemo m)

instance Alternative Memo where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Memo where
    mzero = Memo (const [])
    a `mplus` b = Memo (\ts -> unMemo a ts ++ unMemo b ts)

instance MonadState [Thunk (List Memo Int)] Memo where
    get = Memo (\ts -> [(ts,ts)])
    put ts = Memo (const [((),ts)])

-- memo :: MonadState [Thunk a] m => m a -> m (m a)
memo :: Memo (List Memo Int) -> Memo (Memo (List Memo Int))
memo a = do
    thunks <- get
    let index = length thunks
    put (thunks ++ [Uneval a])
    return $ do
        thunks <- get
        case thunks!!index of
            Eval x -> return x
            Uneval a -> do
                x <- a
                thunks <- get
                let (xs,_:ys) = splitAt index thunks
                put (xs ++ [Eval x] ++ ys)
                return x

{-  This works for simple types like Int, but violates
    law (SHNF) for structures like List.
-}
-- share1 :: Memo t -> Memo (Memo t)
-- share1 a = memo a

{-
Giving up here. The paper is just horribly inconsistent with types.
It mixes up the specificly typed "Memo" with an abstract one, that
would work for all types - which is needed here:
    share x
requires memoization of Int
    share xs
requires memoization if (List Memo Int), for whatever Memo is now.

You could say the paper is non-deterministic about the memoized type :]
-}

-- share :: Memo (List Memo Int) -> Memo (Memo (List Memo Int))
-- share a = memo $ do
--     l <- a
--     case l of
--         Nil -> nil
--         Cons x xs -> do
--             y <- share x
--             ys <- share xs
--             cons y ys

{-
We could now just use the package
    https://hackage.haskell.org/package/explicit-sharing
but it doesn't really give us much for our impl by itself.
-}
