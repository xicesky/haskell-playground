
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sky.Learn.Comonads where

----------------------------------------------------------------------------------------------------
-- Comonad class

class Functor w => Comonad w where
    extract :: w a -> a

    duplicate :: w a -> w (w a)
    duplicate = extend id

    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate
    {- Laws:
        'extract' . 'fmap' f = f . 'extract'
        'fmap' ('fmap' f) . 'duplicate' = 'duplicate' . 'fmap' f
        'extend' f = 'fmap' f . 'duplicate'
    -}

----------------------------------------------------------------------------------------------------

infixr 5 :<
data Stream a = a :< (Stream a)
    --deriving (Show)

instance (Show a) => Show (Stream a) where
    show stream = showMax 5 stream where
        showMax 0 _ = "..."
        showMax i (v :< vs) = show v ++ " :< " ++ showMax (i-1) vs

emptyStream :: a -> Stream a
emptyStream v = v :< (emptyStream v)

myHistory :: Stream String
myHistory = "first" :< "second" :< "third" :< "fourth" :< emptyStream ""

first :: Stream a -> a
first (v :< vs) = v

second :: Stream a -> a
second (v1 :< v2 :< vs) = v2

----------------------------------------------------------------------------------------------------

instance Functor Stream where
    fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (v :< vs) = (f v) :< (fmap f vs)

instance Comonad Stream where
    extract :: Stream a -> a
    extract (v :< _) = v
    duplicate :: Stream a -> Stream (Stream a)
    duplicate all@(v :< vs) = all :< (duplicate vs)
    extend :: (Stream a -> b) -> Stream a -> Stream b
    extend f all@(v :< vs) = (f all) :< (extend f vs)

----------------------------------------------------------------------------------------------------

newtype Kelvin = Kelvin { getKelvin :: Double }
    deriving (Show, Num, Fractional)

newtype Celsius = Celsius { getCelsius :: Double }
    deriving (Show, Num, Fractional)

kelvinToCelsius :: Kelvin -> Celsius
kelvinToCelsius (Kelvin t) = Celsius (t - 273.15)

type Thermostat a = (Kelvin, Kelvin -> a)

initialThermostat :: Thermostat Celsius
initialThermostat = (298.15, kelvinToCelsius)

up :: Thermostat a -> a
up (t, f) = f (t + 1)

down :: Thermostat a -> a
down (t, f) = f (t - 1)

up' :: (Kelvin, Kelvin -> a) -> (Kelvin, Kelvin -> a)
up' (t, f) = (t + 1, f)

toString :: Thermostat Celsius -> String
toString (t, f) = show (getCelsius (f t)) ++ " Celsius"

instance Comonad Thermostat where
    extract :: Thermostat a -> a
    extract (t, f) = f t
    extend :: (Thermostat a -> b) -> Thermostat a -> Thermostat b
    extend view (t, f) = (t, \t' -> view (t', f))

{-
extend (\t' -> p (up' t')) (t, f)
=   (t, \t'' -> (\t' -> p (up' t')) (t'', f))
=   (t, \t'' -> p (up' (t'', f)))
=   (t, \t' -> p (up' (t', f)))
=   (t, \t' -> p (t' + 1, f))

extend p (up' (t, f))
=   extend p (t + 1, f)
=   (t + 1, \t' -> p (t', f))
-}
