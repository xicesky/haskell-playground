
module Sky.Ideas.BoundedNum where

-- Arbitrary, but finite choice [0;upper[
data BoundedWord = BoundedWord
    { _valueW :: Word       -- _valueW < _upperW
    , _upperW :: Word       -- _upperW > 0
    }

-- Arbitrary, but finite choice [lower;upper[
data BoundedInteger = BoundedInteger
    { _valueI :: Integer    -- _lowerI <= _valueI < _upperI
    , _lowerI :: Integer
    , _upperI :: Integer    -- _upperI > _lowerI
    }

convertWI :: BoundedWord -> BoundedInteger
convertWI (BoundedWord v u) = BoundedInt (toInteger v) 0 u

convertIW :: BoundedInteger -> BoundedWord
convertIW (BoundedInteger v l u)
    | l == 0    = BoundedWord (fromInteger v) (fromInteger u)
    | otherwise = error "Lower bound != 0"

shiftI :: Integer -> BoundedInteger -> BoundedInteger
shiftI delta (BoundedInteger i a b) = BoundedInteger (i+delta) (a+delta) (b+delta)

shiftI0 :: BoundedInteger -> BoundedInteger
shiftI0 i = shiftI $ - (_lowerI i)

{-
-- TODO

wrapNegatives :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
--wrapNegatives (min, maxE, i) | (i < min || i >= maxE) = error "FU"
wrapNegatives (a, b, i) = (0, b - a, wrap i) where
    wrap i | i < 0      = i + b - a
    wrap i | otherwise  = i

-}
