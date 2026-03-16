{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Data.Duoid.Laws
  ( Law (..),
    Laws (..),
    getLaws,
    NormalLaws (..),
    getNormalLaws,
    checkLaw,
    interchangeLaw,
    joinUnitLaw,
    splitUnitLaw,
    swapUnitLaw,
    unswapUnitLaw,
  )
where

import "base" Data.Bool (Bool)
import "base" Data.Kind (Type)
import "this" Data.Duoid (Duoid, pempty, sempty, (>->), (|-|))
import "this" Data.Duoid qualified as Duoid (Normal)

-- | A law is represented by two expressions an an operation for comparing their
--   results.
--
-- @since 0.0.1
type Law :: Type -> Type -> Type
data Law a b = Law {cmp :: b -> b -> Bool, x :: a -> b, y :: a -> b}

type role Law representational representational

-- | Ensure that a law holds under a given input.
--
-- @since 0.0.1
checkLaw :: Law a b -> a -> Bool
checkLaw law a = cmp law (x law a) (y law a)

-- | A parallel composition of sequences can be converted to a sequence of
--   compositions.
--
-- @since 0.0.1
interchangeLaw :: (Duoid a) => (a -> a -> Bool) -> Law (a, a, a, a) a
interchangeLaw fn =
  Law
    fn
    (\(a, b, c, d) -> (a >-> b) |-| (c >-> d))
    (\(a, b, c, d) -> (a |-| c) >-> (b |-| d))

-- | A parallel unit can be split into a sequence of parallel units.
--
-- @since 0.0.1
splitUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
splitUnitLaw fn = Law fn (\() -> pempty) (\() -> pempty >-> pempty)

-- | A parallel composition of sequential unit can be joined into a single
--   sequential unit.
--
-- @since 0.0.1
joinUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
joinUnitLaw fn = Law fn (\() -> sempty |-| sempty) (\() -> sempty)

-- | A parallel unit can be converted to a sequential unit.
--
-- @since 0.0.1
swapUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
swapUnitLaw fn = Law fn (\() -> pempty) (\() -> sempty)

-- | The laws for a `Duoid`.
--
-- @since 0.0.1
type Laws :: Type -> Type
data Laws a = Laws
  { splitUnit :: Law () a,
    joinUnit :: Law () a,
    swapUnit :: Law () a,
    interchange :: Law (a, a, a, a) a
  }

type role Laws representational

-- | The `Duoid` laws for a particluar comparison function.
--
-- @since 0.0.1
getLaws :: (Duoid a) => (a -> a -> Bool) -> Laws a
getLaws fn =
  Laws
    { splitUnit = splitUnitLaw fn,
      joinUnit = joinUnitLaw fn,
      swapUnit = swapUnitLaw fn,
      interchange = interchangeLaw fn
    }

-- | Additional law for `Duoid.Normal` duoids. A sequential unit can be
--   converted back to a parallel unit. This forms an isomorphism with
--   `swapUnitLaw`.
--
-- @since 0.0.1
unswapUnitLaw :: (Duoid.Normal a) => (a -> a -> Bool) -> Law () a
unswapUnitLaw fn = Law fn (\() -> sempty) (\() -> pempty)

-- | The laws for a `Duoid.Normal` duoid.
--
-- @since 0.0.1
type NormalLaws :: Type -> Type
data NormalLaws a = NormalLaws
  { duoidLaws :: Laws a,
    unswapUnit :: Law () a
  }

type role NormalLaws representational

-- | The `Duoid.Normal` duoid laws for a particluar comparison function.
--
-- @since 0.0.1
getNormalLaws :: (Duoid.Normal a) => (a -> a -> Bool) -> NormalLaws a
getNormalLaws fn =
  NormalLaws
    { duoidLaws = getLaws fn,
      unswapUnit = unswapUnitLaw fn
    }
