
module Ideas.Lifted where

import Prelude hiding ((&&))
import qualified Prelude as P
import Control.Applicative

-- We usually like to lift operators over functions
class Boo a where
    (&) :: a -> a -> a

instance Boo Bool where
    (&) = (P.&&)

-- instance Boo b => Boo (a -> b) where
--     (&) = liftA2 (&&)

instance (Boo b, Applicative f) => Boo (f b) where
    (&) = liftA2 (&)

-- (&&) now works with predicates like @odd :: Int -> Bool@

-- always returns false
isSpecial :: Int -> Bool
isSpecial = odd & even

{-----------------------------------------------------------------------------}

{- Now the big question, can we generalize this to some
    mechanism to lift ANY binary operator?
-}

class Liftable f where
    lift :: (a -> b -> c) -> f a -> f b -> f c

type family (.) :: (b -> c) -> (a -> b) -> a -> c

-- If only we could write this...
-- instance (Liftable f, Applicative g) => Liftable (f . g) where
--     lift = lift . liftA2

instance Applicative f => Liftable f where
    lift = liftA2

(&&) :: Liftable f => f Bool -> f Bool -> f Bool
(&&) = lift (P.&&)

isSpecial2 :: Int -> Bool
isSpecial2 = odd && even

-- isSpecial3 :: Int -> Int -> Bool
-- isSpecial3 = (==) && (\x y -> x + y == 5)
