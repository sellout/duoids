{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Control.Duoidal.Trans.Class
  ( DuoidalTrans (lift),
    NormalTrans,
  )
where

import "base" Data.Kind (Constraint, Type)
import "duoids" Control.Duoidal (Duoidal)
import "duoids" Control.Duoidal qualified as Duoidal (Normal)

-- | Transformers that preserve `Duoidal`.
--
-- @since 0.0.1
type DuoidalTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
class (forall m. (Duoidal m) => Duoidal (t m)) => DuoidalTrans t where
  -- | Embeds an action of the underlying functor.
  --
  -- @since 0.0.1
  lift :: (Duoidal m) => m a -> t m a

-- | Transformers that preserve `Duoidal.Normal`.
--
-- @since 0.0.1
type NormalTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
class (DuoidalTrans t, forall m. (Duoidal.Normal m) => Duoidal.Normal (t m)) => NormalTrans t
