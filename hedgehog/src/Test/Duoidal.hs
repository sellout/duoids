{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Test.Duoidal
  ( validate,
    validateSequential,
    validateNormal,
  )
where

import safe "base" Control.Applicative ((<*>))
import safe "base" Control.Category (id, (.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Bool (Bool)
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Foldable (elem)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (fst, snd, uncurry)
import safe "base" Data.Typeable (Typeable, showsTypeRep, typeRep)
import safe "base" Text.Show (Show)
import safe "duoids" Control.Duoidal
  ( Duoidal,
    join,
    liftA2,
    liftM2,
    pure,
    return,
    (>>=),
  )
import safe "duoids" Control.Duoidal qualified as Duoidal (Normal)
import safe "duoids" Control.Duoidal.Laws
  ( interchange,
    joinUnit,
    splitUnit,
    swapUnit,
    unswapUnit,
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

-- | `Control.Duoidal.Sequential`’s `Control.Applicative.Applicative` agrees
--   with its `Control.Monad.Monad`, as [`Control.Monad.Monad`’s
--   laws](https://hackage.haskell.org/package/base/docs/Control-Monad.html#t:Monad)
--   require: “@(\<*\>) = ap@”.
apBind :: (Duoidal f, Eq (f (a, b))) => Law (f a, f b) (f (a, b))
apBind =
  Law (==) (uncurry (liftM2 (,))) \(fa, fb) ->
    fa >>= \a -> fb >>= \b -> return (a, b)

parallelProperties ::
  ( Duoidal f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x)
  ) =>
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
parallelProperties genF =
  let genFInt = genF $ Gen.int32 Range.linearBounded
   in [ ( "Parallel associativity",
          propertyFromLaw
            ( Law
                (==)
                ( \(fa, fb, fc) ->
                    fmap (\(a, (b, c)) -> ((a, b), c))
                      . liftA2 (,) fa
                      $ liftA2 (,) fb fc
                )
                \(fa, fb, fc) -> liftA2 (,) (liftA2 (,) fa fb) fc
            )
            $ (,,) <$> genFInt <*> genFInt <*> genFInt
        ),
        ( "Parallel left identity",
          propertyFromLaw
            (Law (==) (fmap snd . liftA2 (,) (pure ())) id)
            genFInt
        ),
        ( "Parallel right identity",
          propertyFromLaw
            (Law (==) (\fa -> fst <$> liftA2 (,) fa (pure ())) id)
            genFInt
        )
      ]
-- Fixing this lint would make each side of a law the same expression.
{-# ANN parallelProperties ("HLint: ignore Use fmap" :: String) #-}
{-# INLINEABLE parallelProperties #-}

sequentialProperties ::
  ( Duoidal f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x)
  ) =>
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
sequentialProperties genF =
  let genFInt = genF $ Gen.int32 Range.linearBounded
   in [ ( "Sequential associativity",
          propertyFromLaw (Law (==) (join . join) (join . fmap join))
            . genF
            $ genF genFInt
        ),
        ( "Sequential left identity",
          propertyFromLaw (Law (==) (join . return) id) genFInt
        ),
        ( "Sequential right identity",
          propertyFromLaw (Law (==) (join . fmap return) id) genFInt
        ),
        ("apBind", propertyFromLaw apBind $ (,) <$> genFInt <*> genFInt)
      ]
-- Fixing this lint would make each side of a law the same expression.
{-# ANN sequentialProperties ("HLint: ignore Use =<<" :: String) #-}
{-# INLINEABLE sequentialProperties #-}

propertyFromLaw :: (Show a) => Law a b -> Hedgehog.Gen a -> Hedgehog.Property
propertyFromLaw law genA =
  Hedgehog.property $ Hedgehog.assert . checkLaw law =<< Hedgehog.forAll genA
{-# INLINEABLE propertyFromLaw #-}

nonNormalProperties ::
  forall f.
  ( Duoidal f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x)
  ) =>
  (forall x. (Eq x) => f x -> f x -> Bool) ->
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  [(Hedgehog.PropertyName, Hedgehog.Property)]
nonNormalProperties rel genF =
  let genInt = Gen.int32 Range.linearBounded
      genFInt = genF genInt
   in parallelProperties genF
        <> sequentialProperties genF
        <> [ ( "interchange",
               propertyFromLaw (interchange rel) $
                 (,,,) <$> genFInt <*> genFInt <*> genFInt <*> genFInt
             ),
             ("splitUnit", propertyFromLaw (splitUnit rel) genInt),
             ("joinUnit", propertyFromLaw (joinUnit rel) genInt),
             ("swapUnit", propertyFromLaw (swapUnit rel) genInt)
           ]
{-# INLINEABLE nonNormalProperties #-}

-- | A property group that checks the `Duoidal` laws for a functor:
--
-- @since 0.0.1
validate ::
  forall f.
  ( Duoidal f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x),
    Typeable f
  ) =>
  -- | relation
  (forall x. (Eq x) => f x -> f x -> Bool) ->
  -- | generator
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  Hedgehog.Group
validate rel genF =
  Hedgehog.Group
    (Hedgehog.GroupName $ "Duoidal " <> showType (Proxy :: Proxy f))
    $ nonNormalProperties rel genF
{-# INLINEABLE validate #-}

-- | A property group that checks that a functor’s `Control.Duoidal.Sequential`
--   is a lawful `Control.Monad.Monad`, agreeing with its
--   `Control.Applicative.Applicative`. These are some of the properties
--   `validate` checks, for a functor that doesn’t satisfy the others, or whose
--   arguments to `validate` are hard to write.
--
-- @since 99999
validateSequential ::
  forall f.
  ( Duoidal f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x),
    Typeable f
  ) =>
  -- | generator
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  Hedgehog.Group
validateSequential genF =
  Hedgehog.Group
    (Hedgehog.GroupName $ "Sequential " <> showType (Proxy :: Proxy f))
    $ sequentialProperties genF

-- | A property group that checks the `Duoidal.Normal` laws for a functor.
--
-- @since 99999
validateNormal ::
  forall f.
  ( Duoidal.Normal f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x),
    Typeable f
  ) =>
  -- | relation
  (forall x. (Eq x) => f x -> f x -> Bool) ->
  -- | generator
  (forall x. Hedgehog.Gen x -> Hedgehog.Gen (f x)) ->
  Hedgehog.Group
validateNormal rel genF =
  Hedgehog.Group
    (Hedgehog.GroupName $ "Duoidal.Normal " <> showType (Proxy :: Proxy f))
    $ ( "unswapUnit",
        propertyFromLaw (unswapUnit rel) $ Gen.int32 Range.linearBounded
      )
      : nonNormalProperties rel genF
{-# INLINEABLE validateNormal #-}
