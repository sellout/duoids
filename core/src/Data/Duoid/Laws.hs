{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
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

-- | An equation, as two functions of a value and a way to compare their
--   results.
--
-- @since 0.0.1
type Law :: Type -> Type -> Type
data Law a b = Law {cmp :: b -> b -> Bool, x :: a -> b, y :: a -> b}

-- | Whether the two sides of a `Law` agree on a value.
--
-- @since 0.0.1
checkLaw :: Law a b -> a -> Bool
checkLaw law a = cmp law (x law a) (y law a)

-- | Parallel and sequential composition interchange.
--
-- @since 0.0.1
interchangeLaw :: (Duoid a) => (a -> a -> Bool) -> Law (a, a, a, a) a
interchangeLaw fn =
  Law
    fn
    (\(a, b, c, d) -> (a >-> b) |-| (c >-> d))
    (\(a, b, c, d) -> (a |-| c) >-> (b |-| d))

-- | The parallel unit is equal to the sequential composition of parallel units.
--
-- @since 0.0.1
splitUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
splitUnitLaw fn = Law fn (\() -> pempty) (\() -> pempty >-> pempty)

-- | The parallel composition of sequential units is equal to the sequential
--   unit.
--
-- @since 0.0.1
joinUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
joinUnitLaw fn = Law fn (\() -> sempty |-| sempty) (\() -> sempty)

-- | The parallel unit relates to the sequential unit.
--
-- @since 0.0.1
swapUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
swapUnitLaw fn = Law fn (\() -> pempty) (\() -> sempty)

-- | The laws that every `Duoid` has to satisfy.
--
-- @since 0.0.1
type Laws :: Type -> Type
data Laws a = Laws
  { splitUnit :: Law () a,
    joinUnit :: Law () a,
    swapUnit :: Law () a,
    interchange :: Law (a, a, a, a) a
  }

-- | The `Laws` for a type, given a way to compare its values.
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

-- | Additional law for `Duoid.Normal` duoids.
--
-- @since 0.0.1
unswapUnitLaw :: (Duoid.Normal a) => (a -> a -> Bool) -> Law () a
unswapUnitLaw fn = Law fn (\() -> sempty) (\() -> pempty)

-- | The laws that every `Duoid.Normal` has to satisfy.
--
-- @since 0.0.1
type NormalLaws :: Type -> Type
data NormalLaws a = NormalLaws
  { duoidLaws :: Laws a,
    unswapUnit :: Law () a
  }

-- | The `NormalLaws` for a type, given a way to compare its values.
--
-- @since 0.0.1
getNormalLaws :: (Duoid.Normal a) => (a -> a -> Bool) -> NormalLaws a
getNormalLaws fn =
  NormalLaws
    { duoidLaws = getLaws fn,
      unswapUnit = unswapUnitLaw fn
    }
