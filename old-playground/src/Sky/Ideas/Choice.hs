
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MagicHash              #-}

module Sky.Ideas.Choice where

import GHC.Integer.Logarithms (integerLog2#)
import GHC.Exts( Int( I# ) )

{- Try to do something like "Enumerable" or "Enum"
    Useful for generating stuff.
-}

ilog2 :: Integer -> Integer
ilog2 i = toInteger (I# (integerLog2# i))

----------------------------------------------------------------------------------------------------

data CH = CH { _i :: Integer, _n :: Integer, _m :: Integer }

instance Show CH where
    show (CH i n m) = show (i,n,m)

chooseIofN :: Integer -> Integer -> CH
chooseIofN i n | i >= n = error "i >= n"
chooseIofN i n = CH i n 1

cof :: Integer -> Integer -> CH
cof = chooseIofN

entropyLeft :: CH -> Integer
entropyLeft (CH i n m) = quot n m

bitsLeft :: CH -> Integer
bitsLeft (CH i n m) =   -- ilog2 (n / m) = (ilog2 n) - (ilog2 m)
    (ilog2 n) - (ilog2 m)

combine :: CH -> CH -> CH
combine (CH i0 n0 m0) (CH i1 n1 m1) = CH i n m where
    i = i0 * m + i1 * n0 * m1
    n = n0 * n1
    m = m0 * m1

extract :: Integer -> CH -> (Integer, CH)
extract c (CH i0 n0 m0) = (i, CH i1 n1 m1) where
    g = gcd n0 c
    c' = c `quot` g
    (i, i1') = quotRem (c * i0) n0
    i1 = i1' * c'
    n1 = n0 `quot` g
    m1 = m0 * c'

c0of1 :: CH
c0of1 = 0 `cof` 1

c0of2 :: CH
c0of2 = 0 `cof` 2

c1of2 :: CH
c1of2 = 1 `cof` 2


{-
----------------------------------------------------------------------------------------------------

combineChoice (i,imax) (j,jmax) = (i + j * imax, imax * jmax)
partialChoice (i,imax) max | max > imax = error $ "Not enough entropy"
partialChoice (i,imax) max

----------------------------------------------------------------------------------------------------

data EntropyAmount
    = FiniteEntropy Integer
    | InfiniteEntropy

-- Much like RNGs, but has well defined ranges, entropy might be finite!
class EntropySource e where
    entropyLeft :: e -> EntropyAmount
    -- = -- fu sublime

    -- | Uniformly choose an integer in range [0..max[
    chooseFiniteInteger :: Integer -> e -> (Maybe Integer, e)
    -- = -- fu sublime

    -- | Uniformly choose an int in range [0..max[
    chooseFiniteInt :: Int -> e -> (Maybe Int, e)
    chooseFiniteInt max entropy = (fmap fromIntegral i, e') where
        (i, e') = chooseInteger (toInteger max)

class Choosable a where
    entropyRequired :: Proxy a -> EntropyAmount
    -- = -- fu sublime

    choose :: EntropySource e => e -> (Maybe a, e)
    -- = -- fu sublime

    chooseFinite :: EntropySource e => a -> e -> (Maybe a, e)
    -- = -- fu sublime
    --chooseFinite max entropy

----------------------------------------------------------------------------------------------------

newtype FiniteEntropySource = FiniteEntropySource
    { _remainder :: Integer
    , _remainMax :: Integer
    , _source :: [Bool]
    }

instance EntropySource FiniteEntropySource where
    entropyAmount (FiniteEntropySource r rmax source) = 2 ^ length source + rmax
    chooseFiniteInteger

----------------------------------------------------------------------------------------------------

instance Chooseable () where
    entropyRequired Proxy = FiniteEntropy 0
    choose entropy = (Just (), entropy)
    chooseFinite () entropy = (Just (), entropy)

instance Chooseable a => Chooseable [a] where

-}