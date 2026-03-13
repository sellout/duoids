{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Control.Monad.Trans.Commutative
  ( CommutativeT (CommutativeT),
    getCommutativeT,
    lift,
    lower,
  )
where

import safe "barbies" Data.Functor.Transformer
  ( ApplicativeT,
    ConstraintsT,
    DistributiveT,
    FunctorT,
    MonadT,
    TraversableT,
    tdistribute,
    tembed,
    tlift,
    tmap,
    ttraverse,
  )
import safe "base" Control.Applicative (Alternative, Applicative)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad (Monad, MonadPlus)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (Functor, fmap)
import safe "base" Data.Kind (Type)
import safe "base" Data.Monoid (Monoid)
import safe "base" Data.Ord (Ord)
import safe "base" Data.Semigroup (Semigroup)
import safe "base" Data.Traversable (Traversable)
import safe "base" GHC.Generics (Generic, Generic1)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
import safe "duoids" Control.Duoidal (DuoidalIO, Parallel, Sequential)
import safe "duoids" Control.Duoidal qualified as Duoidal
import safe "duoids" Control.Monad.Commutative (Commutative (Commutative), getCommutative)
import "newtype" Control.Newtype (Newtype, over)
import safe "transformers" Control.Monad.Trans.Class (MonadTrans)
import safe "base" Prelude
  ( Bounded,
    Enum,
    Floating,
    Fractional,
    Integral,
    Num,
    Real,
    RealFloat,
    RealFrac,
  )

-- | This isn’t a different transformer itself, but a newtype over a transformer
--   to indicate that it’s commutative. This can be used with @DerivingVia@.
--   Also, it doesn’t imply that the entire transformer stack forms a
--   commutative monad. For that, you want
--   `Control.Monad.Commutative.Commutative. This only indicates that @t@ is a
--   commutative transformer.
--
--   For example @`CommutativeT` `MaybeT` `IO`@ is valid, even though `IO` isn’t
--   commutative. This gives the entire transformer stack a `Duoidal` instance
--   (although `MaybeT` already has the same `Duoidal` instance, so it’s not
--   necessary in that case). However, @`Commutative` (`MaybeT` `IO`)@ is /not/
--   valid, because that would require that `IO` be commutative, which it isn’t.
type CommutativeT ::
  forall {k}. ((k -> Type) -> k -> Type) -> (k -> Type) -> k -> Type
newtype CommutativeT t m a = CommutativeT {getCommutativeT :: t m a}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving stock (Foldable, Functor, Generic1, Traversable)
  deriving newtype
    ( Bounded,
      Enum,
      Floating,
      Fractional,
      Integral,
      Monoid,
      Num,
      Real,
      RealFloat,
      RealFrac,
      Semigroup
    )
  deriving newtype (Alternative, Applicative, Monad, MonadPlus)
  deriving newtype (ApplicativeT, ConstraintsT, FunctorT, MonadTrans)

deriving via
  (Parallel (t (m :: Type -> Type) :: Type -> Type))
  instance
    (Functor (t m), Applicative (Parallel (t m))) =>
    Applicative (Parallel (CommutativeT t m))

deriving via
  (Sequential (t (m :: Type -> Type) :: Type -> Type))
  instance
    (Functor (t m), Applicative (Sequential (t m))) =>
    Applicative (Sequential (CommutativeT t m))

deriving via
  (Sequential (t (m :: Type -> Type) :: Type -> Type))
  instance
    (Functor (t m), Monad (Sequential (t m))) =>
    Monad (Sequential (CommutativeT t m))

deriving newtype instance (DuoidalIO (t m)) => DuoidalIO (CommutativeT t m)

deriving newtype instance
  (Duoidal.Normal (t m)) =>
  Duoidal.Normal (CommutativeT t m)

instance (DistributiveT t) => DistributiveT (CommutativeT t) where
  tdistribute = CommutativeT . tdistribute . fmap getCommutativeT

instance (MonadT t) => MonadT (CommutativeT t) where
  tlift = CommutativeT . tlift
  tembed f = over CommutativeT $ tembed (getCommutativeT . f)

instance (TraversableT t) => TraversableT (CommutativeT t) where
  ttraverse f = fmap CommutativeT . ttraverse f . getCommutativeT

instance Newtype (CommutativeT t m a) (t m a)

lift :: (FunctorT t) => CommutativeT t (Commutative m) a -> Commutative (t m) a
lift (CommutativeT tcma) = Commutative $ tmap getCommutative tcma

lower :: (FunctorT t) => Commutative (t m) a -> CommutativeT t (Commutative m) a
lower (Commutative tma) = CommutativeT $ tmap Commutative tma
