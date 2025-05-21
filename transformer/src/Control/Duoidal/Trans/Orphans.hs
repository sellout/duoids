{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Duoidal.Trans.Orphans () where

import "base" Control.Applicative (Applicative, liftA2, pure)
import "base" Control.Category ((.))
import "base" Control.Monad (Monad, (>>=))
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Functor.Compose (Compose (Compose), getCompose)
import "base" Data.Semigroup (Semigroup)
import "duoids" Control.Duoidal
  ( Duoidal,
    Parallel (Parallel, getParallel),
    Sequential (Sequential, getSequential),
  )
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))

instance (Semigroup e, Monad m) => Applicative (Parallel (ExceptT e m)) where
  pure = Parallel . pure

  liftA2 f (Parallel (ExceptT a)) (Parallel (ExceptT b)) =
    Parallel . ExceptT . fmap getParallel . getCompose $
      liftA2 f (Compose (fmap Parallel a)) (Compose (fmap Parallel b))

instance (Semigroup e, Monad m) => Applicative (Sequential (ExceptT e m)) where
  pure = Sequential . pure

  liftA2 f (Sequential a) (Sequential b) = Sequential (liftA2 f a b)

instance (Semigroup e, Monad m) => Monad (Sequential (ExceptT e m)) where
  Sequential a >>= f = Sequential (a >>= getSequential . f)

instance (Semigroup e, Monad m) => Duoidal (ExceptT e m)
