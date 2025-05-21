{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
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
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (($))
import "base" Data.Tuple (uncurry)
import "this" Control.Duoidal (Duoidal, liftA2, liftM2, pure, return)
import "this" Control.Duoidal qualified as Duoidal (Normal)
import "this" Data.Duoid.Laws (Law (Law))

-- commonUnit :: forall f. Iso' (f a) (f a)
-- commonUnit = iso id id

-- data LeafyTree a = Node (LeafyTree a) (LeafyTree a) | Leaf a

-- instance Applicative (Parallel (Either (LeafyTree a))) where
--   pure = Parallel . pure
--   Left as <*> Left bs = Left (Node as bs)
--   Left as <*> Right _ = Left as
--   Right _ <*> Left bs = Left bs
--   Right f <*> Right a = Right $ f a

-- -- Semigroup a => Duoidal.validateNormal @(Either (LeafyTree a)) _ id id commonUnit
-- -- Duoidal.validateNormal @IO _ id id commonUnit
-- -- Duoidal.validateNormal @[] _ id id (iso ((: []) . head) (repeat . head))

-- |
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
interchange ::
  (Duoidal f, Eq (f ((a, c), (b, d)))) =>
  (f ((a, b), (c, d)) -> f ((a, c), (b, d))) ->
  Law (f a, f b, f c, f d) (f ((a, c), (b, d)))
interchange fn =
  Law
    (==)
    (\(fa, fb, fc, fd) -> fn . liftA2 (,) (liftM2 (,) fa fb) $ liftM2 (,) fc fd)
    $ \(fa, fb, fc, fd) -> liftM2 (,) (liftA2 (,) fa fc) $ liftA2 (,) fb fd

splitUnit :: (Duoidal f, Eq (f (a, a))) => (f a -> f (a, a)) -> Law a (f (a, a))
splitUnit fn =
  Law (==) (fn . pure) $ uncurry (liftM2 (,)) . (pure &&& pure)

joinUnit :: (Duoidal f, Eq (f a)) => (f (a, a) -> f a) -> Law a (f a)
joinUnit fn =
  Law
    (==)
    (fn . uncurry (liftA2 (,)) . (return &&& return))
    return

swapUnit :: (Duoidal f, Eq (f a)) => (f a -> f a) -> Law a (f a)
swapUnit fn = Law (==) (fn . pure) return

-- | Additional law for /normal/ duoids.
unswapUnit :: (Duoidal.Normal f, Eq (f a)) => (f a -> f a) -> Law a (f a)
unswapUnit fn = Law (==) (fn . return) pure
