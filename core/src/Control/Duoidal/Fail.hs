{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- NOTE: `HasCallStack` is seen as redundant.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module Control.Duoidal.Fail
  ( fail,
    sequentialFail,
  )
where

import "base" Control.Category ((.))
import "base" Control.Monad (Monad)
import "base" Control.Monad.Fail (MonadFail)
import "base" Control.Monad.Fail qualified as Monad
import "base" Data.Maybe (Maybe)
import "base" Data.Monoid (Ap (Ap))
import "base" Data.String (String)
import "base" GHC.Stack (HasCallStack)
import "base" System.IO (IO)
import "this" Control.Duoidal (Sequential (Sequential), getSequential)

-- | A `Control.Duoidal.Duoidal` version of `Monad.fail`.
--
-- @since 99999
fail :: (HasCallStack, MonadFail (Sequential f)) => String -> f a
fail = getSequential . Monad.fail

-- | Lift the underlying `Monad`’s `fail` to `Control.Duoidal.Duoidal`.
--
-- @since 99999
sequentialFail :: (MonadFail f) => String -> Sequential f a
sequentialFail = Sequential . Monad.fail

-- |
--
-- @since 99999
instance MonadFail (Sequential IO) where
  fail = sequentialFail

-- |
--
-- @since 99999
instance MonadFail (Sequential Maybe) where
  fail = sequentialFail

-- |
--
-- @since 99999
instance
  (MonadFail (Sequential f), Monad (Sequential (Ap f))) =>
  MonadFail (Sequential (Ap f))
  where
  fail = Sequential . Ap . fail
