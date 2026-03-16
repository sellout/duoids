{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- Operations specific to failure duoids.
module Control.Duoidal.Either
  ( leftAccum,
    leftAccum',
    noteAccum,
    noteAccum',
    withInput,
  )
where

import "base" Control.Applicative (Applicative, pure)
import "base" Control.Arrow ((&&&))
import "base" Control.Category (id, (.))
import "base" Data.Bifunctor (first)
import "base" Data.Either (Either (Left))
import "base" Data.Function (($))
import "base" Data.Maybe (Maybe, maybe)
import "base" Data.Tuple (uncurry)

-- | Pair a function’s result with its input.
--
--   I think `traverse` is often used too early. When applying a function to a
--   list of values, it’s the caller that knows whether
--
-- 1. a single failure causes everything to fail,
-- 2. multiple failures need to preserved,
-- 3. order matters, and
-- 4. any success is good enough.
--
-- > Data.Traversable.traverse f
--
--   immediately decides all of these (yes, no, no, no), discarding a bunch of
--   information. `Control.Duoidal.traverse` defers the answer to no. 2 (and
--   partially no. 3), but even that isn’t good enough. It’s better to use
--   something like @`fmap` f@ (or @`fmap` (`withInput` f)@) and defer all of
--   those answers to the caller. Thanks to laziness, that deferral is often
--   free.
--
-- > Map.fromList $ <$>
--
--  __TODO__: This isn’t really specific to `Either`, so it should move
--            somewhere else.
--
-- @since 999999999
withInput :: (a -> b) -> a -> (a, b)
withInput = (id &&&)

-- | Converts a `Maybe`-returning function into a failed-input-accumulating one.
--
--   This is particularly useful for traversals, where you want to track _which_
--   elements of the traversal failed. E.g.,
--
-- > traverse safeHead :: t [a] -> Maybe (t a)
--
--   becomes
--
-- > traverse (noteAccum safeHead) :: t [a] -> Either (NonEmpty a) (t a)
--
-- @since 0.0.1
noteAccum :: (Applicative f) => (a -> Maybe b) -> a -> Either (f a) b
noteAccum = noteAccum' pure

-- | A generalization of `noteAccum` that allows an arbitrary function for
--   producing the error result.
--
-- @since 999999999
noteAccum' :: (a -> e) -> (a -> Maybe b) -> a -> Either e b
noteAccum' e f a = maybe (Left $ e a) pure $ f a

-- | Like `noteAccum`, but for `Either`.
--
-- @since 999999999
leftAccum :: (Applicative f) => (a -> Either e b) -> a -> Either (f (a, e)) b
leftAccum = leftAccum' (\a -> pure . (a,))

-- | A generalization of `leftAccum` that allows an arbitrary function for
--   producing the error result.
--
-- @since 999999999
leftAccum' :: (a -> e -> e') -> (a -> Either e b) -> a -> Either e' b
leftAccum' e f = uncurry ($) . (first . e &&& f)
