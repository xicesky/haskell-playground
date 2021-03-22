
module Sky.ControlLens.ExternalDefs where

-- This is defined externally and we can't modify it
-- How can we make lenses for it?

data Type = TInt | TDouble | TVoid
  deriving (Eq, Show)

data Definition = Definition String Type
  deriving (Eq, Show)

data Module = Module {
    moduleName :: String
  , definitions ::  [Definition]
  }
  deriving (Eq, Show)

-- Let's try some ADTs
data Stuff
  = StuffA {
      name :: String
    , value :: Int
    }
  | StuffB {
      name :: String
    , flag :: Bool
    }
  deriving (Eq, Show)

exampleStuffA :: Stuff
exampleStuffA = StuffA "intvalue" 5
exampleStuffB :: Stuff
exampleStuffB = StuffB "boolflag" True
exampleStuff :: [Stuff]
exampleStuff = [ exampleStuffA, exampleStuffB ]
