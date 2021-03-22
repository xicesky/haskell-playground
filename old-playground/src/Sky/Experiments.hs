
module Sky.Experiments where

-- Ideas:
-- Actual implementations with simple values

data A
data B
data C
data D
data S
data T

data F a    -- Functor TODO
data G a    -- Cofunctor TODO

data M a    -- Monad

uu = undefined

ab = uu :: A -> B

s_ma = uu :: S -> M A
b_mt = uu :: B -> M T

instance Functor M where
    fmap = undefined

instance Applicative M where
    pure = undefined
    (<*>) = undefined

instance Monad M where
    return = undefined
    (>>=) = undefined

