
module Sky.Ideas.ArbIntTree where

{-
    Many data types (all?) could be translated ('encoded') to trees of this form,
    losing a lot of type informations of course.
-}

data ArbIntTree
    = Leaf Integer
    | Node [ArbIntTree]

-- XXX not powerful enough?
aitMap :: (Integer -> a) -> ([a] -> a) -> ArbIntTree -> a
aitMap fLeaf fNode = m where
    m (Leaf x) = fLeaf x
    m (Node xs) = fNode $ fmap m xs

class AIT v where
    toAIT :: v -> ArbIntTree
    fromAIT :: ArbIntTree -> v

instance AIT () where
    toAIT () = Node []
    fromAIT _ = ()

instance Integral v => AIT v where
    toAIT v = Leaf $ toInteger v
    fromAIT (Leaf i) = fromInteger i

instance AIT v => AIT [v] where
    toAIT xs = Node $ fmap toAIT xs
    fromAIT (Node is) = fmap fromAIT is



aitFlatten :: ArbIntTree -> ArbIntTree
aitFlatten x@(Leaf _) = x
aitFlatten (Node xs) = Node $ flatten xs
    flatten (Leaf x : xs) = Leaf 0 : Leaf x : flatten xs
    flatten (Node ns : xs) = Leaf (length ns) : ns ++ flatten xs
