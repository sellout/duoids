{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Data.Monoid.Commutative
  ( Comm (Comm),
  )
where

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

-- | A commutative `Monoid` forms a `Duoid` with itself.
--
--  __NB__: Be careful not to wrap a non-commutative `Monoid` with this newtype.
--
-- @since 999999999
type Comm :: Type -> Type
newtype Comm a = Comm a
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

type role Comm representational

instance Newtype (Comm a) a
