
module Sky.Ideas.MoreIsos where

import Data.Int (Int8)
import Data.Word (Word8)

{- In a programming language that directly supports isos, you could compose
    isomorphisms using "if".
    This function, for example, is isomorphic:
-}

wrapNegatives :: Int8 -> Word8
wrapNegatives i | i >= 0    = fromIntegral i
wrapNegatives i | i < 0     = fromInteger (toInteger i + 256)

{- Because it just wraps negative values to positive, unused ones.
    (Just like negative integers are encoded in binary...)

    How can we compose isomorphisms with conditions?

-}

{- The following function don't have the "proper types", i.e. the types
    are actually "a bit too large". This means there is no proof or
    type safety for it to generate an actual isomorphism.
    (They can be seen as Semi-Isomorphisms)
-}
isoCond1 :: (a -> Bool) -> (b -> Bool) -> Iso a b -> Iso a b -> Iso a b

{- In order to get the types right, they need to be isomorphic to some
    sum type:
-}

isoCond2 :: Iso a (Either b c) -> Iso b b' -> Iso c c' -> Iso a (Either c c')

{- A very interesting paper on isomorphisms is:
    https://arxiv.org/pdf/0808.2953v4.pdf
    Paul Tarau, University of North Texas:
    "Isomorphic Data Encodings in Haskell and their Generalization to
    Hylomorphisms on Hereditarily Finite Data Types"
-}
