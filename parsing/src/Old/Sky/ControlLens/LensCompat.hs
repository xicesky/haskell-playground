
module Old.Sky.ControlLens.LensCompat where

import Control.Lens
-- import Control.Lens.TH (makeFieldOptics, lensRules, _fieldToDef, mappingNamer) -- 4.14
import Control.Lens.TH (makeLensesWith, defaultFieldRules)
import Control.Lens.Internal.FieldTH (DefName(TopName))
import Language.Haskell.TH.Syntax (Name, nameBase, mkName)
import Language.Haskell.TH.Lib (DecsQ)

{-
-- Copied from lens-4.14 ---------------------------------------------------------------------------

type FieldNamer = Name -- ^ Name of the data type that lenses are being generated for.
                  -> [Name] -- ^ Names of all fields (including the field being named) in the data type.
                  -> Name -- ^ Name of the field being named.
                  -> [DefName] -- ^ Name(s) of the lens functions. If empty, no lens is created for that field.

-- | Create a 'FieldNamer' from a mapping function. If the function
-- returns @[]@, it creates no lens for the field.
mappingNamer :: (String -> [String]) -- ^ A function that maps a @fieldName@ to @lensName@s.
             -> FieldNamer
mappingNamer mapper _ _ = fmap (TopName . mkName) . mapper . nameBase
-}

-- Our definitions ---------------------------------------------------------------------------------

directNamer :: FieldNamer
directNamer = mappingNamer return
--simpleNamer _ _ n = [TopName (mkName (nameBase n))]

makeDirectLenses :: Name -> DecsQ
makeDirectLenses = makeLensesWith (defaultFieldRules & lensField .~ directNamer)
