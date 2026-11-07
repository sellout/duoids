{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Control.Duoidal.Laws
  ( interchange,
    joinUnit,
    splitUnit,
    swapUnit,
    unswapUnit,
  )
where

import "base" Control.Arrow ((&&&))
import "base" Control.Category ((.))
import "base" Data.Bool (Bool)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Tuple (fst, uncurry)
import "this" Control.Duoidal (Duoidal, liftA2, liftM2, pure, return)
import "this" Control.Duoidal qualified as Duoidal (Normal)
import "this" Data.Duoid.Laws (Law (Law))

-- $setup
-- >>> import "base" Data.Either (Either (Left, Right))
-- >>> import "base" Data.Eq ((==))
-- >>> import "base" Data.Function (flip)
-- >>> import "base" Data.Functor.Classes (liftEq, liftEq2)
-- >>> import "base" Data.List (isInfixOf)
-- >>> import "base" Data.Maybe (Maybe (Just, Nothing))
-- >>> import "base" Data.Ord ((<=))
-- >>> import "base" Numeric.Natural (Natural)
-- >>> import Data.Duoid.Laws (checkLaw)

-- | Parallel and sequential composition interchange.
--
-- ```
--  |        |
-- / \      / \
-- A C      A C
-- | |  ->  \_/
-- | |      / \
-- B D      B D
-- \ /      \ /
--  |        |
-- ```
--
--   The relation is the interchange morphism, from the parallel composition of
--   sequential ones to the sequential composition of parallel ones. Equality
--   is too strong: when `pure` and `return` agree, Eckmann–Hilton makes the two
--   compositions coincide and commute, so only commutative monads could
--   satisfy it.
--
-- >>> checkLaw (interchange (liftEq (==))) (Just 1, Just 2, Just 3, Nothing)
-- True
-- >>> checkLaw (interchange (liftEq2 (flip isInfixOf) (==))) (Left "a", Left "b", Right 0, Left "c")
-- True
-- >>> checkLaw (interchange (liftEq (==))) (Left "a", Left "b", Right 0, Left "c")
-- False
-- >>> checkLaw (interchange (liftEq2 (<=) (==))) ((1 :: Natural, 'a'), (0, 'b'), (0, 'c'), (1, 'd'))
-- True
--
-- @since 0.0.1
interchange ::
  (Duoidal f) =>
  (f ((a, c), (b, d)) -> f ((a, c), (b, d)) -> Bool) ->
  Law (f a, f b, f c, f d) (f ((a, c), (b, d)))
interchange rel =
  Law
    rel
    ( \(fa, fb, fc, fd) ->
        fmap (\((a, b), (c, d)) -> ((a, c), (b, d)))
          . liftA2 (,) (liftM2 (,) fa fb)
          $ liftM2 (,) fc fd
    )
    $ \(fa, fb, fc, fd) -> liftM2 (,) (liftA2 (,) fa fc) $ liftA2 (,) fb fd
{-# INLINEABLE interchange #-}

-- | The parallel unit relates to the sequential composition of parallel
--   units.
--
-- @since 0.0.1
splitUnit :: (Duoidal f) => (f (a, a) -> f (a, a) -> Bool) -> Law a (f (a, a))
splitUnit rel =
  Law rel (pure . (\a -> (a, a))) $ uncurry (liftM2 (,)) . (pure &&& pure)
{-# INLINEABLE splitUnit #-}

-- | The parallel composition of sequential units relates to the sequential
--   unit.
--
-- @since 0.0.1
joinUnit :: (Duoidal f) => (f a -> f a -> Bool) -> Law a (f a)
joinUnit rel =
  Law rel (fmap fst . uncurry (liftA2 (,)) . (return &&& return)) return
{-# INLINEABLE joinUnit #-}

-- | The parallel unit relates to the sequential unit.
--
-- @since 0.0.1
swapUnit :: (Duoidal f) => (f a -> f a -> Bool) -> Law a (f a)
swapUnit rel = Law rel pure return

-- | Additional law for /normal/ duoidal functors: the sequential unit also
--   relates to the parallel unit.
--
-- @since 0.0.1
unswapUnit :: (Duoidal.Normal f) => (f a -> f a -> Bool) -> Law a (f a)
unswapUnit rel = Law rel return pure
