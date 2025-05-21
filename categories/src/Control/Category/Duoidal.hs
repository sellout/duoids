{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- "Data.Functor.Day" is only inferred `Safe`.
{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}

module Control.Category.Duoidal where

import "base" Control.Category (Category, id, (.))
import "base" Data.Bifunctor (Bifunctor, bimap)
import "base" Data.Either (Either (Left, Right))
import "base" Data.Function (const)
import "base" Data.Functor (Functor, fmap)
import "base" Data.Functor.Compose (Compose (Compose), getCompose)
import "base" Data.Functor.Identity (Identity (Identity), runIdentity)
import "base" Data.Kind (Type)
import "base" Data.Proxy (Proxy)
import "base" Data.Void (Void)
-- import "categories" Control.Category.Associative (Associative)
import "kan-extensions" Data.Functor.Day (Day (Day))
import "base" Prelude (error)

-- Would use the one from “categories”, but it’s insufficiently polymorphic
class
  (Category cat) =>
  Monoidal (cat :: k -> k -> Type) (m :: k -> k -> k)
  where
  type Id cat m :: k
  idl :: cat (m (Id cat m) a) a
  idr :: cat (m a (Id cat m)) a
  coidl :: cat a (m (Id cat m) a)
  coidr :: cat a (m a (Id cat m))
  lassoc :: cat (m a (m b c)) (m (m a b) c)
  rassoc :: cat (m (m a b) c) (m a (m b c))

class (Monoidal cat m) => BraidedMonoidalCategory cat m where
  braid :: m a b `cat` m b a

instance Monoidal (->) (,) where
  type Id (->) (,) = ()
  idl ((), a) = a
  idr (a, ()) = a
  coidl a = ((), a)
  coidr a = (a, ())
  lassoc (a, (b, c)) = ((a, b), c)
  rassoc ((a, b), c) = (a, (b, c))

instance Monoidal (->) Either where
  type Id (->) Either = Void
  idl = \case
    Left _ -> error "unreachable"
    Right a -> a
  idr = \case
    Left a -> a
    Right _ -> error "unreachable"
  coidl a = Right a
  coidr a = Left a
  lassoc = \case
    Left a -> Left (Left a)
    Right (Left b) -> Left (Right b)
    Right (Right c) -> Right c
  rassoc = \case
    Left (Left a) -> Left a
    Left (Right b) -> Right (Left b)
    Right c -> Right (Right c)

instance BraidedMonoidalCategory (->) (,) where
  braid (x, y) = (y, x)

instance BraidedMonoidalCategory (->) Either where
  braid (Left x) = Right x
  braid (Right y) = Left y

class
  (Monoidal cat par, Monoidal cat seq) =>
  DuoidalCategory cat par seq
  where
  interchange :: par (seq a b) (seq c d) `cat` seq (par a c) (par b d)
  splitUnit :: proxy par -> Id cat par `cat` seq (Id cat par) (Id cat par)
  joinUnit :: proxy seq -> par (Id cat seq) (Id cat seq) `cat` Id cat seq
  swapUnit :: proxy par -> proxy' seq -> Id cat par `cat` Id cat seq

-- swapUnit = defaultSwapUnit

-- instance
--   {-# OVERLAPPABLE #-}
--   (BraidedMonoidalCategory cat m) =>
--   DuoidalCategory cat m m
--   where
--   interchange = lassoc . fmap (rassoc . fmap braid . lassoc) . rassoc
--   swapUnit _ _ = id

class (DuoidalCategory cat par seq) => NormalDuoidalCategory cat par seq where
  counit :: proxy par -> proxy' seq -> Id cat seq `cat` Id cat par

-- instance
--   {-# OVERLAPPABLE #-}
--   (BraidedMonoidalCategory cat m) =>
--   NormalDuoidalCategory cat m m
--   where
--   counit _ _ = id

-- This should be implementable by taking the left or right unit of par (I ◇ I),
-- then taking the left and right unit of ★, respectively (J⋆I)⋄(I⋆J)
-- then call `interchange`
-- then collapse the unit back down
defaultSwapUnit ::
  forall par seq proxy proxy'.
  (DuoidalCategory (->) par seq, Bifunctor par, Bifunctor seq) =>
  proxy par ->
  proxy' seq ->
  Id (->) par ->
  Id (->) seq
defaultSwapUnit _ _ =
  idr . bimap idr idl . interchange . bimap @par (coidl @_ @_ @seq) (coidr @_ @_ @seq) . coidl

newtype NaturalTransformation f g = NT {runNT :: forall x. f x -> g x}

instance Category NaturalTransformation where
  id = NT id
  f . g = NT (runNT f . runNT g)

instance Monoidal NaturalTransformation Day where
  type Id NaturalTransformation Day = (->) ()
  idl = NT (\(Day i fx fn) -> fmap (fn (i ())) fx)
  idr = NT (\(Day fx i fn) -> fmap (\x -> fn x (i ())) fx)
  coidl = NT (\fx -> Day id fx (\() -> id))
  coidr = NT (\fx -> Day fx id const)

instance Monoidal NaturalTransformation Compose where
  type Id NaturalTransformation Compose = Identity
  idl = NT (runIdentity . getCompose)
  idr = NT (fmap runIdentity . getCompose)
  coidl = NT (Compose . Identity)
  coidr = NT (Compose . fmap Identity)

instance DuoidalCategory NaturalTransformation Day Compose where
  interchange =
    NT
      ( \(Day f g fn) ->
          Compose
            ( Day
                (getCompose f)
                (getCompose g)
                (\f' g' -> Day f' g' fn)
            )
      )
  splitUnit _ = NT (\id -> Compose (\() -> id))
  joinUnit _ = NT (\(Day (Identity a) (Identity b) fn) -> Identity (fn a b))
  swapUnit _ _ = NT (\fn -> Identity (fn ()))

instance NormalDuoidalCategory NaturalTransformation Day Compose where
  counit _ _ = NT (\(Identity u) () -> u)
