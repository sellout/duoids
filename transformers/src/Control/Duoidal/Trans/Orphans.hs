{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Control.Duoidal.Trans.Orphans () where

import "base" Control.Applicative (Applicative)
import "base" Control.Applicative qualified as Base (liftA2, pure, (<*>))
import "base" Control.Category ((.))
import "base" Control.Monad (Monad)
import "base" Control.Monad qualified as Base ((>>=))
import "base" Data.Either (Either (Left), either)
import "base" Data.Function (($))
import "base" Data.Functor (fmap)
import "base" Data.Functor.Compose (Compose (Compose), getCompose)
import "base" Data.Semigroup (Semigroup)
import "duoids" Control.Duoidal
  ( Duoidal,
    DuoidalIO,
    Parallel (Parallel),
    Sequential (Sequential),
    getParallel,
    getSequential,
    liftA2,
    liftIO,
    pure,
    return,
    (=<<),
  )
import "duoids" Control.Duoidal qualified as Duoidal
import "duoids" Data.Duoid (Duoid, sempty, (>->), (|-|))
import "duoids" Data.Duoid qualified as Duoid (Normal)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import "transformers" Control.Monad.Trans.Writer (WriterT (WriterT), runWriterT)
import "this" Control.Duoidal.Trans.Class (DuoidalTrans, lift)
import "this" Control.Duoidal.Trans.Class qualified as Duoidal (NormalTrans)

instance (Semigroup e, Duoidal m) => Applicative (Parallel (ExceptT e m)) where
  pure = Parallel . lift . pure
  liftA2 f (Parallel (ExceptT a)) (Parallel (ExceptT b)) =
    Parallel
      . ExceptT
      . fmap getParallel
      . getParallel
      . getCompose
      . Base.liftA2 f (Compose . Parallel $ fmap Parallel a)
      . Compose
      . Parallel
      $ fmap Parallel b

instance (Semigroup e, Duoidal m) => Applicative (Sequential (ExceptT e m)) where
  pure = Sequential . ExceptT . return . return
  liftA2 f (Sequential (ExceptT a)) (Sequential (ExceptT b)) =
    Sequential
      . ExceptT
      . fmap getSequential
      . getSequential
      . getCompose
      . Base.liftA2 f (Compose . Sequential $ fmap Sequential a)
      . Compose
      . Sequential
      $ fmap Sequential b

instance (Semigroup e, Duoidal m) => Monad (Sequential (ExceptT e m)) where
  (Sequential (ExceptT m)) >>= k =
    Sequential . ExceptT $
      either (return . Left) (runExceptT . getSequential . k) =<< m

instance (Semigroup e, DuoidalIO m) => DuoidalIO (ExceptT e m) where
  liftIO = lift . liftIO

instance (Semigroup e) => DuoidalTrans (ExceptT e) where
  lift = ExceptT . fmap pure

instance (Semigroup e, Duoidal m) => Duoidal.Normal (ExceptT e m)

instance (Semigroup e) => Duoidal.NormalTrans (ExceptT e)

instance (Duoid w, Duoidal m) => Applicative (Parallel (WriterT w m)) where
  pure = Parallel . lift . pure
  Parallel (WriterT f) <*> Parallel (WriterT v) = Parallel . WriterT $ liftA2 k f v
    where
      k ~(a, w) ~(b, w') = (a b, w |-| w')

instance (Duoid w, Duoidal m) => Applicative (Sequential (WriterT w m)) where
  pure = Sequential . lift . return
  Sequential (WriterT f) <*> Sequential (WriterT v) = Sequential . WriterT $ liftA2 k f v
    where
      k ~(a, w) ~(b, w') = (a b, w >-> w')

instance (Duoid w, Duoidal m) => Monad (Sequential (WriterT w m)) where
  (Sequential (WriterT m)) >>= k = Sequential . WriterT $ Duoidal.do
    ~(a, w) <- m
    ~(b, w') <- runWriterT . getSequential $ k a
    return (b, w >-> w')

instance (Duoid w, DuoidalIO m) => DuoidalIO (WriterT w m) where
  liftIO = lift . liftIO

instance (Duoid w) => DuoidalTrans (WriterT w) where
  lift m = WriterT $ fmap (,sempty) m

instance (Duoidal.Normal m, Duoid.Normal w) => Duoidal.Normal (WriterT w m)

instance (Duoid.Normal w) => Duoidal.NormalTrans (WriterT w)
