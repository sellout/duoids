{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Test.Duoidal
  ( validate,
    validateSequential,
    -- validateNormal,
  )
where

import safe "base" Control.Applicative ((<*>))
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<), (=<<))
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Foldable (elem)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Functor.Classes (Eq1, Show1)
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (uncurry)
import safe "base" Data.Typeable (Typeable, showsTypeRep, typeRep)
import safe "duoids" Control.Duoidal (Duoidal, liftM2, return, (>>=))
-- import safe qualified "duoids" Control.Duoidal as Duoidal (Normal)
import safe "duoids" Control.Duoidal.Laws
  ( interchange,
    joinUnit,
    splitUnit,
    swapUnit,
    --    unswapUnit,
  )
import safe "duoids" Data.Duoid.Laws (Law (Law), checkLaw)
import "hedgehog" Hedgehog qualified
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Internal.Property qualified as Hedgehog
  ( GroupName (GroupName),
  )
import "hedgehog" Hedgehog.Range qualified as Range

showType :: (Typeable a) => proxy a -> String
showType a =
  let typeStr = showsTypeRep (typeRep a) ""
   in if ' ' `elem` typeStr
        then "(" <> typeStr <> ")"
        else typeStr

-- | `Sequential`’s `Applicative` agrees with its `Monad`, as
--   [`Monad`’s laws](https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Monad)
--   require: “@(\<*\>) = ap@”.
apBind :: (Duoidal f, Eq (f (a, b))) => Law (f a, f b) (f (a, b))
apBind =
  Law (==) (uncurry (liftM2 (,))) \(fa, fb) ->
    fa >>= \a -> fb >>= \b -> return (a, b)

apBindProperty ::
  (Duoidal f, Eq1 f, Show1 f) =>
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  (Hedgehog.PropertyName, Hedgehog.Property)
apBindProperty genF =
  let genFInt = genF $ Gen.int32 Range.linearBounded
   in ( "apBind",
        Hedgehog.property
          . (Hedgehog.assert . checkLaw apBind <=< Hedgehog.forAll)
          $ (,) <$> genFInt <*> genFInt
      )

nonNormalProperties ::
  forall f.
  (Duoidal f, Eq1 f, Show1 f) =>
  -- | `interchange`
  (forall x. f ((x, x), (x, x)) -> f ((x, x), (x, x))) ->
  -- | `splitUnit`
  (forall x. f x -> f (x, x)) ->
  -- | `joinUnit`
  (forall x. f (x, x) -> f x) ->
  -- | `swapUnit`
  (forall x. f x -> f x) ->
  -- | generator
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
nonNormalProperties i spu ju swu genF =
  let genInt = Gen.int32 Range.linearBounded
      genFInt = genF genInt
   in [ ( "interchange",
          Hedgehog.property
            . (Hedgehog.assert . checkLaw (interchange i) <=< Hedgehog.forAll)
            $ (,,,) <$> genFInt <*> genFInt <*> genFInt <*> genFInt
        ),
        ( "splitUnit",
          Hedgehog.property $
            Hedgehog.assert . checkLaw (splitUnit spu) =<< Hedgehog.forAll genInt
        ),
        ( "joinUnit",
          Hedgehog.property $
            Hedgehog.assert . checkLaw (joinUnit ju) =<< Hedgehog.forAll genInt
        ),
        ( "swapUnit",
          Hedgehog.property $
            Hedgehog.assert . checkLaw (swapUnit swu) =<< Hedgehog.forAll genInt
        ),
        apBindProperty genF
      ]

-- | A property group that checks the `Duoidal` laws for a functor.
--
-- @since 0.0.1
validate ::
  forall f.
  (Duoidal f, Eq1 f, Show1 f, Typeable f) =>
  -- | `interchange`
  (forall x. f ((x, x), (x, x)) -> f ((x, x), (x, x))) ->
  -- | `splitUnit`
  (forall x. f x -> f (x, x)) ->
  -- | `joinUnit`
  (forall x. f (x, x) -> f x) ->
  -- | `swapUnit`
  (forall x. f x -> f x) ->
  -- | generator
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  Hedgehog.Group
validate i spu ju swu genF =
  Hedgehog.Group
    (Hedgehog.GroupName $ "Duoidal " <> showType (Proxy :: Proxy f))
    $ nonNormalProperties i spu ju swu genF

-- | A property group that checks that a functor’s `Sequential` `Applicative`
--   agrees with its `Monad`. This is one of the properties `validate` checks,
--   for a functor that doesn’t satisfy the others, or whose
--   arguments to `validate` are hard to write.
--
-- @since 99999
validateSequential ::
  forall f.
  (Duoidal f, Eq1 f, Show1 f, Typeable f) =>
  -- | generator
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  Hedgehog.Group
validateSequential genF =
  Hedgehog.Group
    (Hedgehog.GroupName $ "Sequential " <> showType (Proxy :: Proxy f))
    [apBindProperty genF]

-- validateNormal ::
--   forall f.
--   (Duoidal f) =>
--   -- | `interchange`
--   (forall x. f x -> f x) ->
--   -- | `splitUnit`
--   (forall x. f x -> f x) ->
--   -- | `joinUnit`
--   (forall x. f x -> f x) ->
--   -- | `swapUnit` and `unswapUnit`
--   (forall x. Iso' (f x) (f x)) ->
--   -- | generator
--   (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
--   Hedgehog.Group
-- validateNormal i spu ju swu genF =
--   let genFInt = genF Gen.int32
--    in Hedgehog.Group
--         (Hedgehog.GroupName $ "Duoidal.Normal" <> showType (Proxy :: Proxy f))
--         [("unswapUnit", Hedgehog.assert . checkLaw (unswapUnit $ from swu) =<< Hedgehog.forAll genInt)]
