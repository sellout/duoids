{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Test.Duoid
  ( validate,
    validateNormal,
  )
where

import safe "base" Control.Applicative (pure, (<*>))
import safe "base" Control.Category (id, (.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Foldable (elem)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String, fromString)
import safe "base" Data.Typeable (Typeable, showsTypeRep, typeRep)
import safe "base" Text.Show (Show)
import safe "duoids" Data.Duoid (Duoid, pempty, sempty, (>->), (|-|))
import safe "duoids" Data.Duoid qualified as Duoid (Normal)
import safe "duoids" Data.Duoid.Laws
  ( Law (Law),
    checkLaw,
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
{-# INLINEABLE showType #-}

-- | Given a `Law` and a Hedgehog `Hedgehog.Gen` for its input, produces a
--   Hedgehog `Hedgehog.Property` to test the `Law`.
propertyFromLaw ::
  (Show i) => Law i o -> Hedgehog.Gen i -> Hedgehog.Property
propertyFromLaw law gen =
  Hedgehog.property $ Hedgehog.assert . checkLaw law =<< Hedgehog.forAll gen
{-# INLINEABLE propertyFromLaw #-}

-- | The `Data.Semigroup.Semigroup` law for an operation.
semigroupProperties ::
  (Eq a, Show a) =>
  String ->
  (a -> a -> a) ->
  Hedgehog.Gen a ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
semigroupProperties name op genA =
  [ ( fromString $ name <> " associativity",
      propertyFromLaw
        ( Law
            (==)
            (\(a, b, c) -> (a `op` b) `op` c)
            \(a, b, c) -> a `op` (b `op` c)
        )
        $ (,,) <$> genA <*> genA <*> genA
    )
  ]
{-# INLINEABLE semigroupProperties #-}

-- | The `Data.Monoid.Monoid` laws for an operation and its unit.
monoidProperties ::
  (Eq a, Show a) =>
  String ->
  (a -> a -> a) ->
  a ->
  Hedgehog.Gen a ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
monoidProperties name op unit genA =
  semigroupProperties name op genA
    <> [ ( fromString $ name <> " left identity",
           propertyFromLaw (Law (==) (unit `op`) id) genA
         ),
         ( fromString $ name <> " right identity",
           propertyFromLaw (Law (==) (`op` unit) id) genA
         )
       ]
{-# INLINEABLE monoidProperties #-}

nonNormalProperties ::
  forall a.
  (Duoid a, Eq a, Show a) =>
  Duoid.Laws a ->
  Hedgehog.Gen a ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
nonNormalProperties laws genA =
  monoidProperties "Par" (|-|) pempty genA
    <> monoidProperties "Seq" (>->) sempty genA
    <> nonNormalDuoidProperties laws genA
{-# INLINEABLE nonNormalProperties #-}

nonNormalDuoidProperties ::
  forall a.
  (Show a) =>
  Duoid.Laws a ->
  Hedgehog.Gen a ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
nonNormalDuoidProperties laws genA =
  [ ( "interchange",
      propertyFromLaw (interchange laws) $
        (,,,) <$> genA <*> genA <*> genA <*> genA
    ),
    ("splitUnit", propertyFromLaw (splitUnit laws) $ pure ()),
    ("joinUnit", propertyFromLaw (joinUnit laws) $ pure ()),
    ("swapUnit", propertyFromLaw (swapUnit laws) $ pure ())
  ]
{-# INLINEABLE nonNormalDuoidProperties #-}

-- | A property group that checks the `Duoid` laws for a type.
--
-- @since 0.0.1
validate ::
  forall a.
  (Duoid a, Eq a, Show a, Typeable a) =>
  (a -> a -> Bool) ->
  Hedgehog.Gen a ->
  Hedgehog.Group
validate fn genA =
  Hedgehog.Group (Hedgehog.GroupName $ "Duoid " <> showType genA) $
    nonNormalProperties (Duoid.getLaws fn) genA
{-# INLINEABLE validate #-}

-- | A property group that checks the `Duoid.Normal` laws for a type.
--
-- @since 0.0.1
validateNormal ::
  forall a.
  (Duoid.Normal a, Eq a, Show a, Typeable a) =>
  (a -> a -> Bool) ->
  Hedgehog.Gen a ->
  Hedgehog.Group
validateNormal fn genA =
  let laws = Duoid.getNormalLaws fn
   in Hedgehog.Group
        (Hedgehog.GroupName $ "Duoid.Normal " <> showType (Proxy :: Proxy a))
        $ ( "unswapUnit",
            propertyFromLaw (unswapUnit laws) $ pure ()
          )
          : nonNormalProperties (duoidLaws laws) genA
{-# INLINEABLE validateNormal #-}
