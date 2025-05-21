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

type Law :: Type -> Type -> Type
data Law a b = Law {cmp :: b -> b -> Bool, x :: a -> b, y :: a -> b}

checkLaw :: Law a b -> a -> Bool
checkLaw law a = cmp law (x law a) (y law a)

interchangeLaw :: (Duoid a) => (a -> a -> Bool) -> Law (a, a, a, a) a
interchangeLaw fn =
  Law
    fn
    (\(a, b, c, d) -> (a >-> b) |-| (c >-> d))
    (\(a, b, c, d) -> (a |-| c) >-> (b |-| d))

splitUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
splitUnitLaw fn = Law fn (\() -> pempty) (\() -> pempty >-> pempty)

joinUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
joinUnitLaw fn = Law fn (\() -> sempty |-| sempty) (\() -> sempty)

swapUnitLaw :: (Duoid a) => (a -> a -> Bool) -> Law () a
swapUnitLaw fn = Law fn (\() -> pempty) (\() -> sempty)

type Laws :: Type -> Type
data Laws a = Laws
  { splitUnit :: Law () a,
    joinUnit :: Law () a,
    swapUnit :: Law () a,
    interchange :: Law (a, a, a, a) a
  }

getLaws :: (Duoid a) => (a -> a -> Bool) -> Laws a
getLaws fn =
  Laws
    { splitUnit = splitUnitLaw fn,
      joinUnit = joinUnitLaw fn,
      swapUnit = swapUnitLaw fn,
      interchange = interchangeLaw fn
    }

-- | Additional law for `Duoid.Normal` duoids.
unswapUnitLaw :: (Duoid.Normal a) => (a -> a -> Bool) -> Law () a
unswapUnitLaw fn = Law fn (\() -> sempty) (\() -> pempty)

type NormalLaws :: Type -> Type
data NormalLaws a = NormalLaws
  { duoidLaws :: Laws a,
    unswapUnit :: Law () a
  }

getNormalLaws :: (Duoid.Normal a) => (a -> a -> Bool) -> NormalLaws a
getNormalLaws fn =
  NormalLaws
    { duoidLaws = getLaws fn,
      unswapUnit = unswapUnitLaw fn
    }
