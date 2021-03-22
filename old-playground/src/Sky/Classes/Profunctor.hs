
module Sky.Classes.Profunctor where

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

-- There could be much more here
