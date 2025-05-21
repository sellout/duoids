{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Operations specific to failure duoids.
module Control.Duoidal.Either
  ( noteAccum,
  )
where

import "base" Control.Applicative (Applicative, pure)
import "base" Control.Category ((.))
import "base" Data.Either (Either (Left))
import "base" Data.Function (($))
import "base" Data.Maybe (Maybe, maybe)
import "this" Control.Duoidal (Parallel (Parallel))

-- | Converts a maybe-returning function into a failed-input-accumulating one.
--
--   This is particularly useful for traversals, where you want to track _which_
--   elements of the traversal failed. E.g.,
--
-- > traverse safeHead :: t [a] -> Maybe (t a)
--
--   becomes
--
-- > traverse (noteAccum safeHead) :: t [a] -> Parallel (Either (NonEmpty a)) (t a)
noteAccum :: (Applicative f) => (a -> Maybe b) -> a -> Parallel (Either (f a)) b
noteAccum f a = Parallel . maybe (Left $ pure a) pure $ f a
