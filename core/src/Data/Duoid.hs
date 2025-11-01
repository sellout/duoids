{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-methods:sconcat #-}

-- | Provides duoidal operations on functors. This lets us easily mix and match "parallel" and
--  "sequential" operations on structures that have multiple viable `Applicative` instances, like
--  `Either` (`Data.Either.Validation.Validation`) and `System.IO.IO`
--  (`Control.Concurrent.Async.Concurrently`).
--
--   So, for example, when using this, you should ignore the existence of
--  `Data.Either.Validation.Validation`, and always work in `Either` (and `ExceptT`), then, using
--   these operators instead of the usual `Applicative` and `Monad` operators, you will have
--   behavior that correctly mixes the accumulation of errors with the monadic "first failure"
--   semantics. This should lawfully always do what you want, without running into the
--  "`Applicative` semantics must match `Monad` semantics" problem.
--
-- * ♢ – `Parallel`
-- * ★ – `Sequential`
--
--   Resources:
--
-- * https://ncatlab.org/nlab/show/duoidal+category
-- * https://blogs.ncl.ac.uk/andreymokhov/united-monoids/
module Data.Duoid
  ( Duoid,
    Normal,
    Par (..),
    Seq (..),
    pempty,
    sempty,
    (<|>),
    (<->),
  )
where

import "base" Data.Eq ((==))
import "base" Data.Function (($))
import "base" Data.Kind (Constraint, Type)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Ord (max, (<), (>))
import "base" Data.Ratio (Ratio, Rational, (%))
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Word (Word, Word16, Word32, Word64, Word8)
import "base" GHC.Real (infinity)
import "base" Numeric.Natural (Natural)
import "base" Prelude (Bounded, Integral, maxBound, minBound, (+))

type Par :: Type -> Type
newtype Par a = Par {getPar :: a}

type Seq :: Type -> Type
newtype Seq a = Seq {getSeq :: a}

type Duoid :: Type -> Constraint
class (Monoid (Par a), Monoid (Seq a)) => Duoid a

type Normal :: Type -> Constraint
class (Duoid a) => Normal a

pempty :: (Duoid a) => a
pempty = getPar mempty

sempty :: (Duoid a) => a
sempty = getSeq mempty

(<|>) :: (Duoid a) => a -> a -> a
x <|> y = getPar (Par x <> Par y)

(<->) :: (Duoid a) => a -> a -> a
x <-> y = getSeq (Seq x <> Seq y)

-- TODO: add instances for simplicial complexes
--      (https://blogs.ncl.ac.uk/andreymokhov/united-monoids/) and multisets
--      (https://blogs.ncl.ac.uk/andreymokhov/united-monoids/#comment-20714)

instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Semigroup (Par a) where
  Par x <> Par y = Par $ max x y

instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Monoid (Par a) where
  mempty = Par minBound

-- | Saturating addition.
instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Semigroup (Seq a) where
  Seq x <> Seq y =
    let z = x + y
     in Seq $
          if y < 0
            then
              if z > x
                then minBound
                else z
            else
              if z < x
                then maxBound
                else z

instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Monoid (Seq a) where
  mempty = Seq 0

-- | This is the max-plus duoid.
instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Duoid a

instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Semigroup (Par (Ratio a)) where
  Par x <> Par y = Par $ max x y

instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Monoid (Par (Ratio a)) where
  mempty = Par (minBound % 1)

instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Semigroup (Seq (Ratio a)) where
  Seq x <> Seq y =
    let z = x + y
     in Seq $
          if y < 0
            then
              if z > x
                then minBound % 1
                else z
            else
              if z < x
                then maxBound % 1
                else z

instance Semigroup (Seq (Ratio Word8)) where
  Seq x <> Seq y =
    let z = x + y
     in Seq $
          if y < 0
            then
              if z > x
                then minBound % 1
                else z
            else
              if z < x
                then maxBound % 1
                else z

instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Monoid (Seq (Ratio a)) where
  mempty = Seq 0

-- | This is the max-plus duoid
instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Duoid (Ratio a)

instance Normal (Ratio Word)

instance Normal (Ratio Word16)

instance Normal (Ratio Word32)

instance Normal (Ratio Word64)

instance Normal (Ratio Word8)

instance Semigroup (Par Rational) where
  Par x <> Par y = Par $ max x y

instance Monoid (Par Rational) where
  mempty = Par (-infinity)

instance Semigroup (Seq Rational) where
  Seq x <> Seq y =
    Seq $
      if x == (-infinity)
        then
          if y == infinity
            then 0
            else -infinity
        else
          if x == infinity
            then
              if y == (-infinity)
                then 0
                else infinity
            else
              if y == (-infinity)
                then -infinity
                else
                  if y == infinity
                    then infinity
                    else x + y

instance Monoid (Seq Rational) where
  mempty = Seq 0

-- | This is the max-plus duoid
instance Duoid Rational

instance Semigroup (Par Natural) where
  Par x <> Par y = Par $ max x y

instance Monoid (Par Natural) where
  mempty = Par 0

instance Semigroup (Seq Natural) where
  Seq x <> Seq y = Seq $ x + y

instance Monoid (Seq Natural) where
  mempty = Seq 0

-- | This is the max-plus duoid
instance Duoid Natural

-- | This is the max-plus duoid
instance Normal Natural

-- | Saturating addition.
instance Semigroup (Seq Word) where
  Seq x <> Seq y =
    let z = x + y
     in Seq $
          if z < max x y
            then maxBound
            else z

-- | This is the max-plus duoid.
instance Normal Word

-- | Saturating addition.
instance Semigroup (Seq Word16) where
  Seq x <> Seq y =
    let z = x + y
     in Seq $
          if z < max x y
            then maxBound
            else z

-- | This is the max-plus duoid.
instance Normal Word16

-- | Saturating addition.
instance Semigroup (Seq Word32) where
  Seq x <> Seq y =
    let z = x + y
     in Seq $
          if z < max x y
            then maxBound
            else z

-- | This is the max-plus duoid.
instance Normal Word32

-- | Saturating addition.
instance Semigroup (Seq Word64) where
  Seq x <> Seq y =
    let z = x + y
     in Seq $
          if z < max x y
            then maxBound
            else z

-- | This is the max-plus duoid.
instance Normal Word64

-- | Saturating addition.
instance Semigroup (Seq Word8) where
  Seq x <> Seq y =
    let z = x + y
     in Seq $
          if z < max x y
            then maxBound
            else z

-- | This is the max-plus duoid.
instance Normal Word8
