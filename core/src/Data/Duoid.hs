{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- ## resources
--
-- - https://blogs.ncl.ac.uk/andreymokhov/united-monoids/
module Data.Duoid
  ( Duoid,
    Normal,
    Par (Par, getPar),
    Seq (Seq, getSeq),
    pempty,
    sempty,
    (|-|),
    (>->),
    Comm (Comm),
  )
where

import "base" Control.Applicative (liftA2)
import "base" Control.Category ((.))
import "base" Data.Eq (Eq, (==))
import "base" Data.Foldable (Foldable)
import "base" Data.Function (const, ($))
import "base" Data.Functor (Functor)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Ord (Ord, max, (<), (<=), (>))
import "base" Data.Ratio (Ratio, Rational, (%))
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Traversable (Traversable)
import "base" Data.Word (Word, Word16, Word32, Word64, Word8)
import "base" GHC.Real (infinity)
import "base" Numeric.Natural (Natural)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "base" Prelude (Bounded, Integral, maxBound, minBound, (+))

-- | A wrapper to allow specifying a `Monoid` for the parallel (♢) component of
--   a `Duoid`.
type Par :: Type -> Type
newtype Par a = Par {getPar :: a}
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A wrapper to allow specifying a `Monoid` for the sequential (★) component
--   of a `Duoid`.
type Seq :: Type -> Type
newtype Seq a = Seq {getSeq :: a}
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | Instances for `Duoid` are automatically coalesced from the respective
--   @`Monoid` `.` `Par`@ and @`Monoid` `.` `Seq`@ instances.
type Duoid :: Type -> Constraint
class (Monoid (Par a), Monoid (Seq a)) => Duoid a

-- | The automatic `Duoid` instance. This is overlappable in case it doesn’t
--   suffice in specific cases.
instance {-# OVERLAPPABLE #-} (Monoid (Par a), Monoid (Seq a)) => Duoid a

-- | A duoid where there is a natural transformation between the parallel and
--   sequential units. In this category, that is when the units are identical.
type Normal :: Type -> Constraint
class (Duoid a) => Normal a

-- | The parallel unit of a `Duoid`
pempty :: (Duoid a) => a
pempty = getPar mempty

-- | The sequential unit of a `Duoid`.
sempty :: (Duoid a) => a
sempty = getSeq mempty

-- | The parallel operation of a `Duoid`.
(|-|) :: (Duoid a) => a -> a -> a
x |-| y = getPar (Par x <> Par y)

-- | The sequential operation of a `Duoid`.
(>->) :: (Duoid a) => a -> a -> a
x >-> y = getSeq (Seq x <> Seq y)

-- | A commutative `Monoid` forms a `Duoid` with itself.
--
--  __NB__: Be careful not to wrap a non-commutative `Monoid` with this newtype.
type Comm :: Type -> Type
newtype Comm a = Comm a
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance (Semigroup a) => Semigroup (Par (Comm a)) where
  Par (Comm x) <> Par (Comm y) = Par . Comm $ x <> y

instance (Monoid a) => Monoid (Par (Comm a)) where
  mempty = Par $ Comm mempty

instance (Semigroup a) => Semigroup (Seq (Comm a)) where
  Seq (Comm x) <> Seq (Comm y) = Seq . Comm $ x <> y

instance (Monoid a) => Monoid (Seq (Comm a)) where
  mempty = Seq $ Comm mempty

-- max-plus duoids

-- | Maximum.
instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Semigroup (Par a) where
  Par x <> Par y = Par $ max x y

-- | Maximum.
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

-- | Saturating addition.
instance {-# OVERLAPPABLE #-} (Bounded a, Integral a) => Monoid (Seq a) where
  mempty = Seq 0

-- | Maximum.
instance
  {-# OVERLAPPABLE #-}
  (Bounded a, Integral a) =>
  Semigroup (Par (Ratio a))
  where
  Par x <> Par y = Par $ max x y

-- | Maximum.
instance
  {-# OVERLAPPABLE #-}
  (Bounded a, Integral a) =>
  Monoid (Par (Ratio a))
  where
  mempty = Par . (minBound %) $ if (minBound :: a) <= 0 then 1 else maxBound

-- | Saturating addition.
instance
  {-# OVERLAPPABLE #-}
  (Bounded a, Integral a) =>
  Semigroup (Seq (Ratio a))
  where
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

-- | Saturating addition.
instance
  {-# OVERLAPPABLE #-}
  (Bounded a, Integral a) =>
  Monoid (Seq (Ratio a))
  where
  mempty = Seq 0

instance Normal (Ratio Word)

instance Normal (Ratio Word16)

instance Normal (Ratio Word32)

instance Normal (Ratio Word64)

instance Normal (Ratio Word8)

-- | Maximum.
instance Semigroup (Par (Ratio Natural)) where
  Par x <> Par y = Par $ max x y

-- | Maximum.
instance Monoid (Par (Ratio Natural)) where
  mempty = Par 0

-- | Saturating addition.
instance Semigroup (Seq (Ratio Natural)) where
  Seq x <> Seq y = Seq $ x + y

-- | Saturating addition.
instance Monoid (Seq (Ratio Natural)) where
  mempty = Seq 0

-- | Maximum.
instance Semigroup (Par Rational) where
  Par x <> Par y = Par $ max x y

-- | Maximum.
instance Monoid (Par Rational) where
  mempty = Par (-infinity)

-- | Saturating addition.
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

-- | Saturating addition.
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

-- | This is the max-plus duoid.
instance Normal Natural

-- | This is the max-plus duoid.
instance Normal (Ratio Natural)

-- | This is the max-plus duoid.
instance Normal Word

-- | This is the max-plus duoid.
instance Normal Word16

-- | This is the max-plus duoid.
instance Normal Word32

-- | This is the max-plus duoid.
instance Normal Word64

-- | This is the max-plus duoid.
instance Normal Word8

-- tuples

instance Semigroup (Par ()) where
  Par () <> Par () = Par ()

instance Monoid (Par ()) where
  mempty = Par ()

instance Semigroup (Seq ()) where
  Seq () <> Seq () = Seq ()

instance Monoid (Seq ()) where
  mempty = Seq ()

instance Normal ()

instance (Duoid a, Duoid b) => Semigroup (Par (a, b)) where
  Par (a, b) <> Par (a', b') = Par (a |-| a', b |-| b')

instance (Duoid a, Duoid b) => Monoid (Par (a, b)) where
  mempty = Par (pempty, pempty)

instance (Duoid a, Duoid b) => Semigroup (Seq (a, b)) where
  Seq (a, b) <> Seq (a', b') = Seq (a >-> a', b >-> b')

instance (Duoid a, Duoid b) => Monoid (Seq (a, b)) where
  mempty = Seq (sempty, sempty)

instance (Normal a, Normal b) => Normal (a, b)

instance (Duoid a, Duoid b, Duoid c) => Semigroup (Par (a, b, c)) where
  Par (a, b, c) <> Par (a', b', c') = Par (a |-| a', b |-| b', c |-| c')

instance (Duoid a, Duoid b, Duoid c) => Monoid (Par (a, b, c)) where
  mempty = Par (pempty, pempty, pempty)

instance (Duoid a, Duoid b, Duoid c) => Semigroup (Seq (a, b, c)) where
  Seq (a, b, c) <> Seq (a', b', c') = Seq (a >-> a', b >-> b', c >-> c')

instance (Duoid a, Duoid b, Duoid c) => Monoid (Seq (a, b, c)) where
  mempty = Seq (sempty, sempty, sempty)

instance (Normal a, Normal b, Normal c) => Normal (a, b, c)

instance
  (Duoid a, Duoid b, Duoid c, Duoid d) =>
  Semigroup (Par (a, b, c, d))
  where
  Par (a, b, c, d) <> Par (a', b', c', d') =
    Par (a |-| a', b |-| b', c |-| c', d |-| d')

instance (Duoid a, Duoid b, Duoid c, Duoid d) => Monoid (Par (a, b, c, d)) where
  mempty = Par (pempty, pempty, pempty, pempty)

instance
  (Duoid a, Duoid b, Duoid c, Duoid d) =>
  Semigroup (Seq (a, b, c, d))
  where
  Seq (a, b, c, d) <> Seq (a', b', c', d') =
    Seq (a >-> a', b >-> b', c >-> c', d >-> d')

instance (Duoid a, Duoid b, Duoid c, Duoid d) => Monoid (Seq (a, b, c, d)) where
  mempty = Seq (sempty, sempty, sempty, sempty)

instance (Normal a, Normal b, Normal c, Duoid d) => Normal (a, b, c, d)

-- functions

instance (Duoid b) => Semigroup (Par (a -> b)) where
  Par f <> Par g = Par $ liftA2 (|-|) f g

instance (Duoid b) => Monoid (Par (a -> b)) where
  mempty = Par $ const pempty

instance (Duoid b) => Semigroup (Seq (a -> b)) where
  Seq f <> Seq g = Seq $ liftA2 (>->) f g

instance (Duoid b) => Monoid (Seq (a -> b)) where
  mempty = Seq $ const sempty

instance (Normal b) => Normal (a -> b)
