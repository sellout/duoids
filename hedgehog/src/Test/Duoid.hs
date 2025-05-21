{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Test.Duoid
  ( validate,
    validateNormal,
  )
where

import safe "base" Control.Applicative ((<*>))
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Foldable (elem)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Typeable (Typeable, showsTypeRep, typeRep)
import safe "base" Text.Show (Show)
import safe "duoids" Data.Duoid (Duoid)
import safe "duoids" Data.Duoid qualified as Duoid (Normal)
import safe "duoids" Data.Duoid.Laws
  ( checkLaw,
    duoidLaws,
    interchange,
    joinUnit,
    splitUnit,
    swapUnit,
    unswapUnit,
  )
import safe "duoids" Data.Duoid.Laws qualified as Duoid
import "hedgehog" Hedgehog qualified
import "hedgehog" Hedgehog.Internal.Property qualified as Hedgehog
  ( GroupName (GroupName),
  )

showType :: (Typeable a) => proxy a -> String
showType a =
  let typeStr = showsTypeRep (typeRep a) ""
   in if ' ' `elem` typeStr
        then "(" <> typeStr <> ")"
        else typeStr

nonNormalProperties ::
  forall a.
  (Show a) =>
  Duoid.Laws a ->
  Hedgehog.Gen a ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
nonNormalProperties laws genA =
  [ ( "interchange",
      Hedgehog.property
        . (Hedgehog.assert . checkLaw (interchange laws) <=< Hedgehog.forAll)
        $ (,,,) <$> genA <*> genA <*> genA <*> genA
    ),
    ("splitUnit", Hedgehog.property . Hedgehog.assert $ checkLaw (splitUnit laws) ()),
    ("joinUnit", Hedgehog.property . Hedgehog.assert $ checkLaw (joinUnit laws) ()),
    ("swapUnit", Hedgehog.property . Hedgehog.assert $ checkLaw (swapUnit laws) ())
  ]

validate ::
  forall a.
  (Duoid a, Show a, Typeable a) =>
  (a -> a -> Bool) ->
  Hedgehog.Gen a ->
  Hedgehog.Group
validate fn genA =
  Hedgehog.Group (Hedgehog.GroupName $ "Duoid " <> showType genA) $
    nonNormalProperties (Duoid.getLaws fn) genA

validateNormal ::
  forall a.
  (Duoid.Normal a, Show a, Typeable a) =>
  (a -> a -> Bool) ->
  Hedgehog.Gen a ->
  Hedgehog.Group
validateNormal fn genA =
  let laws = Duoid.getNormalLaws fn
   in Hedgehog.Group
        (Hedgehog.GroupName $ "Duoid.Normal " <> showType (Proxy :: Proxy a))
        $ ( "unswapUnit",
            Hedgehog.property . Hedgehog.assert $ checkLaw (unswapUnit laws) ()
          )
          : nonNormalProperties (duoidLaws laws) genA
