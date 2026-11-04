{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module Control.Duoidal.Fix
  ( mfix,
    sequentialMfix,
  )
where

import "base" Control.Category ((.))
import "base" Control.Monad (Monad)
import "base" Control.Monad.Fix (MonadFix)
import "base" Control.Monad.Fix qualified as Monad
import "base" Data.Function (($))
import "base" Data.Maybe (Maybe)
import "base" Data.Monoid (Ap (Ap), getAp)
import "base" System.IO (IO)
import "this" Control.Duoidal (Sequential (Sequential), getSequential)

-- | A `Control.Duoidal.Duoidal` version of `Monad.fail`.
--
-- @since 99999
mfix :: (MonadFix (Sequential f)) => (a -> f a) -> f a
mfix f = getSequential . Monad.mfix $ Sequential . f

-- | Lift the underlying `Monad`’s `mfix` to `Control.Duoidal.Duoidal`.
--
-- @since 99999
sequentialMfix :: (MonadFix f) => (a -> Sequential f a) -> Sequential f a
sequentialMfix f = Sequential . Monad.mfix $ getSequential . f

-- |
--
-- @since 99999
instance MonadFix (Sequential IO) where
  mfix = sequentialMfix

-- |
--
-- @since 99999
instance MonadFix (Sequential Maybe) where
  mfix = sequentialMfix

-- |
--
-- @since 99999
instance
  (MonadFix (Sequential f), Monad (Sequential (Ap f))) =>
  MonadFix (Sequential (Ap f))
  where
  mfix f = Sequential . Ap . mfix $ getAp . getSequential . f
