{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Control.Duoidal.Trans.Class
  ( DuoidalTrans (lift),
    NormalTrans,
  )
where

import safe "barbies" Data.Functor.Transformer (FunctorT, tmap)
import safe "base" Control.Applicative (Applicative)
import safe "base" Control.Applicative qualified as Base (pure, (<*>))
import safe "base" Control.Category ((.))
import safe "base" Control.Monad (Monad)
import safe "base" Control.Monad qualified as Base ((>>=))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, fmap)
import safe "base" Data.Functor.Compose (Compose (Compose), getCompose)
import safe "base" Data.Kind (Constraint, Type)
import safe "base" Data.Maybe (Maybe (Nothing), maybe)
import safe "base" Data.Semigroup (Semigroup)
import safe "duoids" Control.Duoidal
  ( Duoidal,
    DuoidalIO,
    Parallel (Parallel),
    Sequential (Sequential),
    ap,
    getParallel,
    getSequential,
    liftA2,
    liftIO,
    pure,
    return,
    (<*>),
    (=<<),
  )
import safe "duoids" Control.Duoidal qualified as Duoidal
import safe "duoids" Data.Duoid (Duoid, sempty, (>->), (|-|))
import safe "duoids" Data.Duoid qualified as Duoid (Normal)
import safe "transformers" Control.Monad.Trans.Class (MonadTrans)
import safe "transformers" Control.Monad.Trans.Class qualified as MonadTrans
import safe "transformers" Control.Monad.Trans.Except
  ( ExceptT (ExceptT),
    runExceptT,
  )
import safe "transformers" Control.Monad.Trans.Identity
  ( IdentityT (IdentityT),
    runIdentityT,
  )
import safe "transformers" Control.Monad.Trans.Maybe
  ( MaybeT (MaybeT),
    runMaybeT,
  )
import safe "transformers" Control.Monad.Trans.Reader
  ( ReaderT (ReaderT),
    runReaderT,
  )
import safe "transformers" Control.Monad.Trans.Writer
  ( WriterT (WriterT),
    runWriterT,
  )
import safe "this" Control.Monad.Trans.Commutative (CommutativeT (CommutativeT))

type DuoidalTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
class (forall m. (Duoidal m) => Duoidal (t m)) => DuoidalTrans t where
  lift :: (Duoidal m) => m a -> t m a

type NormalTrans :: ((Type -> Type) -> Type -> Type) -> Constraint
class
  (DuoidalTrans t, forall m. (Duoidal.Normal m) => Duoidal.Normal (t m)) =>
  NormalTrans t

-- `CommutativeT`

instance
  ( FunctorT t,
    MonadTrans t,
    forall m. (Duoidal m) => Duoidal (t m),
    forall m. (Functor m) => Functor (t m)
  ) =>
  DuoidalTrans (CommutativeT t)
  where
  lift = CommutativeT . tmap getSequential . MonadTrans.lift . Sequential

instance
  ( FunctorT t,
    MonadTrans t,
    forall m. (Duoidal m) => Duoidal (t m),
    forall m. (Duoidal.Normal m) => Duoidal.Normal (t m),
    forall m. (Functor m) => Functor (t m)
  ) =>
  NormalTrans (CommutativeT t)

-- | The `Parallel` version of `Base.<*>` defined for a common transformer
--   shape.
liftedAp ::
  (Duoidal m, Applicative f) =>
  (forall x. m (f x) -> t m x) ->
  (forall x. t m x -> m (f x)) ->
  Parallel (t m) (a -> b) ->
  Parallel (t m) a ->
  Parallel (t m) b
liftedAp trans runTrans f a =
  Parallel . trans . getParallel . getCompose $
    Compose (Parallel . runTrans $ getParallel f)
      Base.<*> Compose (Parallel . runTrans $ getParallel a)

-- | The `Sequential` version of `Base.ap` defined for a common transformer
--   shape.
liftedAp' ::
  (Duoidal m, Applicative f) =>
  (forall x. m (f x) -> t m x) ->
  (forall x. t m x -> m (f x)) ->
  Sequential (t m) (a -> b) ->
  Sequential (t m) a ->
  Sequential (t m) b
liftedAp' trans runTrans f a =
  Sequential . trans . getSequential . getCompose $
    Compose (Sequential . runTrans $ getSequential f)
      -- This is implemented using `Base.<*>`, so we can use `Compose`.
      Base.<*> Compose (Sequential . runTrans $ getSequential a)

-- `ExceptT`

instance (Semigroup e) => DuoidalTrans (ExceptT e) where
  lift = ExceptT . fmap pure

instance (Semigroup e) => NormalTrans (ExceptT e)

instance (Semigroup e, Duoidal m) => Applicative (Parallel (ExceptT e m)) where
  pure = Parallel . lift . pure
  (<*>) = liftedAp ExceptT runExceptT

instance (Semigroup e, Duoidal m) => Applicative (Sequential (ExceptT e m)) where
  pure = Sequential . lift . return
  (<*>) = liftedAp' ExceptT runExceptT

instance (Semigroup e, Duoidal m) => Monad (Sequential (ExceptT e m)) where
  Sequential (ExceptT m) >>= k =
    Sequential . ExceptT $
      either (return . Left) (runExceptT . getSequential . k) =<< m

instance (Semigroup e, Duoidal m) => Duoidal.Normal (ExceptT e m)

-- `IdentityT`

instance (Duoidal m) => Applicative (Parallel (IdentityT m)) where
  pure = Parallel . lift . pure
  Parallel (IdentityT f) <*> Parallel (IdentityT a) =
    Parallel . IdentityT $ f <*> a

instance (Duoidal m) => Applicative (Sequential (IdentityT m)) where
  pure = Sequential . lift . pure
  Sequential (IdentityT f) <*> Sequential (IdentityT a) =
    Sequential . IdentityT $ ap f a

instance (Duoidal m) => Monad (Sequential (IdentityT m)) where
  Sequential (IdentityT m) >>= k =
    Sequential . IdentityT $ runIdentityT . getSequential . k =<< m

instance (Duoidal m) => Duoidal.Normal (IdentityT m)

deriving via (CommutativeT IdentityT) instance DuoidalTrans IdentityT

deriving via (CommutativeT IdentityT) instance NormalTrans IdentityT

-- `MaybeT`

instance (Duoidal m) => Applicative (Parallel (MaybeT m)) where
  pure = Parallel . lift . pure
  (<*>) = liftedAp MaybeT runMaybeT

instance (Duoidal m) => Applicative (Sequential (MaybeT m)) where
  pure = Sequential . lift . return
  (<*>) = liftedAp' MaybeT runMaybeT

instance (Duoidal m) => Monad (Sequential (MaybeT m)) where
  Sequential (MaybeT m) >>= k =
    Sequential . MaybeT $
      maybe (return Nothing) (runMaybeT . getSequential . k) =<< m

instance (Duoidal m) => Duoidal.Normal (MaybeT m)

deriving via (CommutativeT MaybeT) instance DuoidalTrans MaybeT

deriving via (CommutativeT MaybeT) instance NormalTrans MaybeT

-- `ReaderT`

deriving via (CommutativeT (ReaderT r)) instance DuoidalTrans (ReaderT r)

deriving via (CommutativeT (ReaderT r)) instance NormalTrans (ReaderT r)

instance (Duoidal m) => Applicative (Parallel (ReaderT r m)) where
  pure = Parallel . lift . pure
  Parallel (ReaderT f) <*> Parallel (ReaderT v) =
    Parallel . ReaderT $ \x ->
      getParallel $ Parallel (f x) Base.<*> Parallel (v x)

instance (Duoidal m) => Applicative (Sequential (ReaderT r m)) where
  pure = Sequential . lift . return
  Sequential (ReaderT f) <*> Sequential (ReaderT v) =
    Sequential . ReaderT $ \x ->
      getSequential $ Sequential (f x) Base.<*> Sequential (v x)

instance (Duoidal m) => Monad (Sequential (ReaderT r m)) where
  Sequential (ReaderT m) >>= k = Sequential . ReaderT $ \x ->
    getSequential $
      Sequential (m x) Base.>>= \y ->
        Sequential $ runReaderT (getSequential $ k y) x

instance (DuoidalIO m) => DuoidalIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (Duoidal.Normal m) => Duoidal.Normal (ReaderT r m)

-- `WriterT` (all of the orphans rely on the classes defined here)

instance (Duoid w) => DuoidalTrans (WriterT w) where
  lift m = WriterT $ fmap (,sempty) m

instance (Duoid.Normal w) => NormalTrans (WriterT w)

instance (Duoid w, Duoidal m) => Applicative (Parallel (WriterT w m)) where
  pure = Parallel . lift . pure
  Parallel (WriterT f) <*> Parallel (WriterT v) =
    Parallel . WriterT $ liftA2 k f v
    where
      k ~(a, w) ~(b, w') = (a b, w |-| w')

instance (Duoid w, Duoidal m) => Applicative (Sequential (WriterT w m)) where
  pure = Sequential . lift . return
  Sequential (WriterT f) <*> Sequential (WriterT v) =
    Sequential . WriterT $ liftA2 k f v
    where
      k ~(a, w) ~(b, w') = (a b, w >-> w')

instance (Duoid w, Duoidal m) => Monad (Sequential (WriterT w m)) where
  (Sequential (WriterT m)) >>= k = Sequential . WriterT $ Duoidal.do
    ~(a, w) <- m
    ~(b, w') <- runWriterT . getSequential $ k a
    return (b, w >-> w')

instance (Duoid.Normal w, Duoidal.Normal m) => Duoidal.Normal (WriterT w m)

instance (Duoid w, DuoidalIO m) => DuoidalIO (WriterT w m) where
  liftIO = lift . liftIO
