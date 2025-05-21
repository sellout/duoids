{-# LANGUAGE Safe #-}

module Control.Duoidal.Laws
  ( interchange,
    joinUnit,
    splitUnit,
    swapUnit,
    -- unswapUnit,
  )
where

import "base" Control.Applicative (liftA2, pure)
import "base" Control.Arrow ((&&&))
import "base" Control.Category ((.))
-- import "base" Control.Duoidal (Duoidal, Parallel (..), Sequential (..))
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (($))
import "base" Data.Tuple (uncurry)
import "this" Control.Duoidal
  ( Duoidal,
    Parallel (Parallel, getParallel),
    Sequential (Sequential, getSequential),
  )
-- import qualified "this" Control.Duoidal as Duoidal (Normal)
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
    ( \(fa, fb, fc, fd) ->
        fn
          . getParallel
          . liftA2 (,) (Parallel . getSequential . liftA2 (,) (Sequential fa) $ Sequential fb)
          . Parallel
          . getSequential
          . liftA2 (,) (Sequential fc)
          $ Sequential fd
    )
    $ \(fa, fb, fc, fd) ->
      getSequential
        . liftA2 (,) (Sequential . getParallel . liftA2 (,) (Parallel fa) $ Parallel fc)
        . Sequential
        . getParallel
        . liftA2 (,) (Parallel fb)
        $ Parallel fd

splitUnit :: (Duoidal f, Eq (f (a, a))) => (f a -> f (a, a)) -> Law a (f (a, a))
splitUnit fn =
  Law (==) (fn . getParallel . pure) $
    getSequential
      . uncurry (liftA2 (,))
      . (Sequential . getParallel . pure &&& Sequential . getParallel . pure)

joinUnit :: (Duoidal f, Eq (f a)) => (f (a, a) -> f a) -> Law a (f a)
joinUnit fn =
  Law
    (==)
    ( fn
        . getParallel
        . uncurry (liftA2 (,))
        . (Parallel . getSequential . pure &&& Parallel . getSequential . pure)
    )
    $ getSequential . pure

swapUnit :: (Duoidal f, Eq (f a)) => (f a -> f a) -> Law a (f a)
swapUnit fn = Law (==) (fn . getParallel . pure) $ getSequential . pure

-- -- | Additional law for /normal/ duoids.
-- unswapUnit :: (Duoidal.Normal f, Eq (f a)) => (f a -> f a) -> Law a (f a)
-- unswapUnit fn = Law (==) (fn . runSequential . pure) $ runParallel . pure
