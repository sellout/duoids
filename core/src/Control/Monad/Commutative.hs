{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Commutative `Monad`s.
module Control.Monad.Commutative
  ( Commutative (Commutative),
  )
where

import safe "base" Control.Applicative (Alternative, Applicative)
import safe "base" Control.Monad (Monad, MonadPlus)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Functor (Functor)
import safe "base" Data.Kind (Type)
import safe "base" Data.Monoid (Monoid)
import safe "base" Data.Ord (Ord)
import safe "base" Data.Semigroup (Semigroup)
import safe "base" Data.Traversable (Traversable)
import safe "base" GHC.Generics (Generic, Generic1)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
import "newtype" Control.Newtype (Newtype)
import safe "base" Prelude
  ( Bounded,
    Enum,
    Floating,
    Fractional,
    Integral,
    Num,
    Real,
    RealFloat,
    RealFrac,
  )

-- | Commutative `Monad`s form a `Duoidal` functor with themselves.
--
--   You can use this with @DerivingVia@ to create instances for your own
--   commutative `Monad`s.
--
--   For existing types, instances for types in base should be available here,
--   but those for other packages may not exist. For those that don’t, you can
--
-- 1. wrap the type in `Commutative` when you need the instance,
-- 2. define orphan instances that look like the instances for `Commutative`, or
-- 3. use the provided operations (like `commutativeAp`) directly.
--
--   Some examples of commutative monads:
-- - those isomorphic to `Identity` (many newtypes fall into this bucket)
-- - reader (@->@)
-- - `Maybe`
-- - `Proxy`
--
--   You can also wrap types that already have `Duoidal` instances in
--   `Commutative` as well (as long as they have an unwrapped `Monad` instance).
--   If their existing `Duoidal` instance isn’t the commutative one, the
--   `Commutative` wrapper will give you the commutative one. I don’t know if
--   this one is correct, but if you have a commutative writer (say, @`Writer`
--   (`Set` `Char`)@), wrapping it in `Commutative` would give you a `Duoidal`
--   instance that behaves commutatively. However, @`Set` a@ should already have
--   a commutative `Duoid` instance, so I don’t think it actually buys you
--   anything.
--
--  __NB__: Don’t use this newtype to turn a non-commutative `Monad` into a
--          duoid.
--
-- @since 999999999
type Commutative :: forall {k}. (k -> Type) -> k -> Type
newtype Commutative f a = Commutative (f a)
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving stock (Foldable, Functor, Generic1, Traversable)
  deriving newtype
    ( Bounded,
      Enum,
      Floating,
      Fractional,
      Integral,
      Monoid,
      Num,
      Real,
      RealFloat,
      RealFrac,
      Semigroup
    )
  deriving newtype (Alternative, Applicative, Monad, MonadPlus)

type role Commutative representational nominal

instance Newtype (Commutative f a) (f a)
