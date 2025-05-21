{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- Provides duoidal operations on functors. This lets us easily mix and match
-- "parallel" and "sequential" operations on structures that have multiple
-- viable `Applicative` instances, like `Either`
-- (`Data.Either.Validation.Validation`) and `System.IO.IO`
-- (`Control.Concurrent.Async.Concurrently`).
--
-- So, for example, when using this, you should ignore the existence of
-- `Data.Either.Validation.Validation`, and always work in `Either` (and
-- `ExceptT`), then, using these operators instead of the usual `Applicative`
-- and `Monad` operators, you will have behavior that correctly mixes the
-- accumulation of errors with the monadic "first failure" semantics. This
-- should lawfully always do what you want, without running into the
-- “`Applicative` semantics must match `Monad` semantics” problem.
--
-- - ♢ – `Parallel`
-- - ★ – `Sequential`
--
-- Because of the historical duplication of `Applicative` (`pure`, `<*`,
-- `sequenceA`) and `Monad` (`return`, `<<`, `sequence`) operations, we don’t
-- need to come up with new names for everything. We just make the definitions
-- actually distinct, whereas they previously had to align.
--
-- __NB__: It’s easy to use the wrong operations when this module is imported,
--         because they have the same names as ones that already exist in
--         @base@. Here are some recommendations on how to use it successfully.
--      1. If you import everything qualified anyway and use `NoDefaultPrelude`,
--         great – qualify this module, and use it alongside qualified
--         `Applicative` and `Monad` operations.
--      2. If you use unqualified imports, I recommend importing this module
--         unqualified /with no import list/. You will likely get errors like
--         either “ambigious occurrence of …” or “couldn’t satisfy constraint
--         Duoidal …”. When the former happens, you can either hide or qualify
--         the import of the non-duoidal operation. When the latter happens, add
--         a qualified import of the non-duoidal operation. The reason for
--         preferring the operations from this module is because they are more
--         restricted and, when they apply, they are likely to do the right
--         thing. If the other operations are preferred, your likely to find
--         yourself with monadic semantics when you were hoping to get duoidal
--         semantics.
--
-- This module supports various combinations of @do@-notation for `Duoid`
-- instances.
--
-- The ideal use case for this module is in combination with @ApplicativeDo@ and
-- either @QualifiedDo@ (for case-by-case @do@ semantics) or @RebindableSyntax@
-- (for module-wide @do@ semantics).
--
--  ## Example
--
--   When the independent steps succeed, we just return the dependent result,
--   like a `Monad`.
--
-- >>> :{
-- Duoidal.do
--   x <- Right "x"
--   y <- Right "y"
--   Left ["Couldn't merge records " <> x <> " and " <> y]
-- :}
-- Left ["Couldn't merge records x and y"]
--
--   But when the independent steps fail, we collect all the independent
--   failures, like `Validation`.
--
-- >>> :{
-- Duoidal.do
--   x <- Left ["Couldn't find record x"]
--   y <- Left ["Couldn't find record y"]
--   Left ["Couldn't merge records " <> x <> " and " <> y]
-- :}
-- Left ["Couldn't find record x","Couldn't find record y"]
--
--  __NB__: `fail` isn’t part of the `Duoidal` interface, so this just
--          re-exports the one from `MonadFail`.
--
-- ## resources
--
-- - https://ncatlab.org/nlab/show/duoidal+category
-- - https://blogs.ncl.ac.uk/andreymokhov/united-monoids/
module Control.Duoidal
  ( Duoidal,
    Normal,
    DuoidalIO (liftIO),

    -- * parallel `Applicative` operations
    Parallel (Parallel, getParallel),
    (<*>),
    (<**>),
    (*>),
    (<*),
    bisequenceA,
    bitraverse,
    liftA2,
    liftA3,
    pure,
    traverse,
    traverse_,
    for,
    for_,
    sequenceA,
    sequenceA_,
    forever,
    unless,
    when,

    -- * sequential `Monad` operations
    Sequential (Sequential, getSequential),
    (>>),
    (>>=),
    (>=>),
    (=<<),
    (<=<),
    join,
    ap,
    (<<>>),
    return,
    (<<),
    bisequence,
    bimapM,
    liftM2,
    liftM3,
    mapM,
    mapM_,
    forM,
    forM_,
    sequence,
    sequence_,
    forever',
    unless',
    when',

    -- * re-exported operations for @QualifiedDo@
    fmap,

    -- * instance helpers
    normalPure,
    sequentialBind,
    sequentialLiftA2,
    sequentialPure,

    -- * duoids from commutative `Monad`s
    Commutative (Commutative, getCommutative),
  )
where

import "base" Control.Applicative (Alternative, Applicative, empty, (<|>))
import "base" Control.Applicative qualified as Base
  ( liftA2,
    liftA3,
    pure,
    (*>),
    (<*),
    (<**>),
    (<*>),
  )
import "base" Control.Category ((.))
import "base" Control.Monad (Monad)
import "base" Control.Monad qualified as Base
  ( forever,
    join,
    liftM2,
    return,
    unless,
    when,
    (<=<),
    (=<<),
    (>=>),
    (>>=),
  )
import "base" Data.Bifunctor (bimap, first)
import "base" Data.Bitraversable (Bitraversable)
import "base" Data.Bitraversable qualified as Base
  ( bimapM,
    bisequence,
    bitraverse,
  )
import "base" Data.Bool (Bool)
import "base" Data.Either (Either (Left, Right), either)
import "base" Data.Eq (Eq)
import "base" Data.Foldable (Foldable)
import "base" Data.Foldable qualified as Base (for_, traverse_)
import "base" Data.Function (const, ($))
import "base" Data.Functor (Functor, fmap)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Traversable (Traversable)
import "base" Data.Traversable qualified as Base (for, traverse)
import "base" GHC.TypeError (ErrorMessage (Text), TypeError)
import "base" System.IO (IO)
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "this" Control.Duoidal.Async qualified as Async
import "this" Data.Duoid (Duoid, pempty, sempty, (>->), (|-|))
import "this" Data.Duoid qualified as Duoid (Normal)
import "base" Prelude (error)

-- $setup
-- >>> :seti -XApplicativeDo
-- >>> :seti -XQualifiedDo
-- >>> import "duoids" Control.Duoidal qualified as Duoidal

type Parallel :: forall {k}. (k -> Type) -> k -> Type
newtype Parallel f a = Parallel {getParallel :: f a}
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- TODO: Replace this with `Unsatisfiable` and `unsatisfiable` once we no longer
--       support GHC 9.6. (And remove the `Applicative` constraint.)
instance
  ( TypeError
      ('Text "Can’t have a `Monad` instance over the `Parallel` newtype."),
    Applicative (Parallel f)
  ) =>
  Monad (Parallel f)
  where
  (>>=) = error "unreachable"

type Sequential :: forall {k}. (k -> Type) -> k -> Type
newtype Sequential f a = Sequential {getSequential :: f a}
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A `Duoidal` functor is a `Monad`, but is also an `Applicative` functor in
--   two ways. One is the `Applicative` induced by the `Monad`, but the other is
--   a `Parallel` `Applicative`, which relates to the monadic (“sequential”)
--   `Applicative` via the duoid laws.
--
--  __NB__: Instances are automatically coalesced from the `Sequential` `Monad`
--          and `Parallel` `Applicative` instances.
type Duoidal :: (Type -> Type) -> Constraint
class (Functor f, Applicative (Parallel f), Monad (Sequential f)) => Duoidal f

-- interchange :: f a -> f a
-- splitUnit :: f a -> f (a, a)
-- joinUnit :: f (a, a) -> f a
-- swapUnit :: f a -> f a
-- swapUnit = defaultSwapUnit

instance
  (Functor f, Applicative (Parallel f), Monad (Sequential f)) =>
  Duoidal f

-- I→≅(J⋆I)⋄(I⋆J)→(J⋄I)⋆(I⋄J)→≅J

-- defaultSwapUnit :: (Duoidal f) => f a -> f a
-- defaultSwapUnit =
--   to rightIdentity --                                           J
--     . bimap (to rightIdentity) (to leftIdentity) --           J ★ J
--     . interchange --                                    (J ♢ I) ★ (I ♢ J)
--     . bimap (from leftIdentity) (from rightIdentity) -- (J ★ I) ♢ (I ★ J)
--     . from leftIdentity --                                    I ♢ I

-- | `Normal` `Duoidal` functors are ones where the two identities (`pure` and
--   `return`) are isomorphic.
type Normal :: (Type -> Type) -> Constraint
class (Duoidal f) => Normal f

--   where
--   -- | This must form an isomorphism with `swapUnit`.
--   unswapUnit :: f a -> f a

-- | The `Parallel` `Applicative` version of `Base.pure`.
pure :: (Duoidal f) => a -> f a
pure = getParallel . Base.pure

return :: (Duoidal f) => a -> f a
return = getSequential . Base.pure

-- | The `Parallel` `Applicative` version of `Base.liftA2`.
liftA2 :: (Duoidal f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a = getParallel . Base.liftA2 f (Parallel a) . Parallel

liftM2 :: (Duoidal f) => (a -> b -> c) -> f a -> f b -> f c
liftM2 f a = getSequential . Base.liftA2 f (Sequential a) . Sequential

-- | The `Parallel` `Applicative` version of `Base.liftA3`.
liftA3 :: (Duoidal f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b = getParallel . Base.liftA3 f (Parallel a) (Parallel b) . Parallel

liftM3 :: (Duoidal f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftM3 f a b =
  getSequential . Base.liftA3 f (Sequential a) (Sequential b) . Sequential

-- | The `Parallel` `Applicative` version of `Base.<*>`.
(<*>) :: (Duoidal f) => f (a -> b) -> f a -> f b
f <*> g = getParallel $ Parallel f Base.<*> Parallel g

infixl 4 <*>

ap :: (Duoidal f) => f (a -> b) -> f a -> f b
ap f g = getSequential $ Sequential f Base.<*> Sequential g

-- | The `Parallel` `Applicative` version of `Base.<**>`.
(<**>) :: (Duoidal f) => f a -> f (a -> b) -> f b
f <**> g = getParallel $ Parallel f Base.<**> Parallel g

infixl 4 <**>

-- |
--
--  __NB__: This is missing from "Control.Monad", but added here to maintain the
--         pairwise definitions of `Parallel` and `Sequential` `Applicative`
--         operations.
(<<>>) :: (Duoidal f) => f a -> f (a -> b) -> f b
f <<>> g = getSequential $ Sequential f Base.<**> Sequential g

infixl 1 <<>>

-- | The `Parallel` `Applicative` version of `Base.*>`.
(*>) :: (Duoidal f) => f a -> f b -> f b
a *> b = getParallel $ Parallel a Base.*> Parallel b

infixl 4 *>

(>>) :: (Duoidal f) => f a -> f b -> f b
a >> b = getSequential (Sequential a Base.*> Sequential b)

infixl 1 >>

-- | The `Parallel` `Applicative` version of `Base.<*`.
(<*) :: (Duoidal f) => f a -> f b -> f a
a <* b = getParallel $ Parallel a Base.<* Parallel b

infixl 4 <*

-- |
--
--  __NB__: This is missing from "Control.Monad", but added here to maintain the
--         pairwise definitions of `Parallel` and `Sequential` `Applicative`
--         operations.
(<<) :: (Monad f) => f a -> f b -> f a
(<<) = (Base.<*)

infixl 1 <<

-- | The `Parallel` `Applicative` version of `Base.bisequenceA`.
bisequenceA :: (Bitraversable t, Duoidal f) => t (f a) (f b) -> f (t a b)
bisequenceA = getParallel . Base.bisequence . bimap Parallel Parallel

-- |
--
--  __NB__: In "Data.Bitraversable", this is identical to `Base.bisequenceA`
--          (it’s not overconstrained to `Monad`), but here we give it
--          sequential semantics, to parallel the difference between `sequenceA`
--          and `sequence`.
bisequence :: (Bitraversable t, Duoidal f) => t (f a) (f b) -> f (t a b)
bisequence = getSequential . Base.bisequence . bimap Sequential Sequential

-- | The `Parallel` `Applicative` version of `Base.bitraverse`.
bitraverse ::
  (Bitraversable t, Duoidal f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bitraverse f g = getParallel . Base.bitraverse (Parallel . f) (Parallel . g)

-- |
--
--  __NB__: In "Data.Bitraversable", this is identical to `Base.bitraverse`
--          (it’s not overconstrained to `Monad`), but here we give it
--          sequential semantics, to parallel the difference between `sequenceA`
--          and `sequence`.
bimapM ::
  (Bitraversable t, Duoidal f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bimapM f g = getSequential . Base.bimapM (Sequential . f) (Sequential . g)

-- | The `Parallel` `Applicative` version of `Base.traverse`.
traverse :: (Traversable t, Duoidal f) => (a -> f b) -> t a -> f (t b)
traverse f = getParallel . Base.traverse (Parallel . f)

mapM :: (Traversable t, Duoidal f) => (a -> f b) -> t a -> f (t b)
mapM f = getSequential . Base.traverse (Sequential . f)

traverse_ :: (Foldable t, Duoidal f) => (a -> f b) -> t a -> f ()
traverse_ f = getParallel . Base.traverse_ (Parallel . f)

mapM_ :: (Foldable t, Duoidal f) => (a -> f b) -> t a -> f ()
mapM_ f = getSequential . Base.traverse_ (Sequential . f)

for :: (Traversable t, Duoidal f) => t a -> (a -> f b) -> f (t b)
for a f = getParallel . Base.for a $ Parallel . f

forM :: (Traversable t, Duoidal f) => t a -> (a -> f b) -> f (t b)
forM a f = getSequential . Base.for a $ Sequential . f

for_ :: (Foldable t, Duoidal f) => t a -> (a -> f b) -> f ()
for_ a f = getParallel . Base.for_ a $ Parallel . f

forM_ :: (Foldable t, Duoidal f) => t a -> (a -> f b) -> f ()
forM_ a f = getSequential . Base.for_ a $ Sequential . f

-- | The `Parallel` `Applicative` version of `Base.sequenceA`.
sequenceA :: (Traversable t, Duoidal f) => t (f a) -> f (t a)
sequenceA = getParallel . Base.traverse Parallel

sequence :: (Traversable t, Duoidal f) => t (f a) -> f (t a)
sequence = getSequential . Base.traverse Sequential

-- | The `Parallel` `Applicative` version of `Base.sequenceA`.
sequenceA_ :: (Foldable t, Duoidal f) => t (f a) -> f ()
sequenceA_ = getParallel . Base.traverse_ Parallel

sequence_ :: (Foldable t, Duoidal f) => t (f a) -> f ()
sequence_ = getSequential . Base.traverse_ Sequential

forever :: (Duoidal f) => f a -> f b
forever = getParallel . Base.forever . Parallel

forever' :: (Duoidal f) => f a -> f b
forever' = getSequential . Base.forever . Sequential

-- | The `Parallel` `Applicative` version of `Base.unless`.
unless :: (Duoidal f) => Bool -> f () -> f ()
unless b = getParallel . Base.unless b . Parallel

unless' :: (Duoidal f) => Bool -> f () -> f ()
unless' b = getSequential . Base.unless b . Sequential

-- | The `Parallel` `Applicative` version of `Base.when`.
when :: (Duoidal f) => Bool -> f () -> f ()
when b = getParallel . Base.when b . Parallel

when' :: (Duoidal f) => Bool -> f () -> f ()
when' b = getSequential . Base.when b . Sequential

join :: (Duoidal f) => f (f a) -> f a
join = getSequential . Base.join . Sequential . fmap Sequential

(>>=) :: (Duoidal f) => f a -> (a -> f b) -> f b
a >>= f = getSequential (Sequential a Base.>>= Sequential . f)

infixr 1 >>=

(=<<) :: (Duoidal f) => (a -> f b) -> f a -> f b
f =<< a = getSequential (Sequential . f Base.=<< Sequential a)

infixr 1 =<<

(>=>) :: (Duoidal f) => (a -> f b) -> (b -> f c) -> a -> f c
f >=> g = getSequential . (Sequential . f Base.>=> Sequential . g)

infixr 1 >=>

(<=<) :: (Duoidal f) => (b -> f c) -> (a -> f b) -> a -> f c
f <=< g = getSequential . (Sequential . f Base.<=< Sequential . g)

infixr 1 <=<

type DuoidalIO :: (Type -> Type) -> Constraint
class (Duoidal m) => DuoidalIO m where
  liftIO :: IO a -> m a

normalPure :: (Applicative f) => a -> Parallel f a
normalPure = Parallel . Base.pure

sequentialPure :: (Monad f) => a -> Sequential f a
sequentialPure = Sequential . Base.return

sequentialLiftA2 ::
  (Monad f) =>
  (a -> b -> c) ->
  Sequential f a ->
  Sequential f b ->
  Sequential f c
sequentialLiftA2 f (Sequential a) = Sequential . Base.liftA2 f a . getSequential

sequentialBind ::
  (Monad f) => Sequential f a -> (a -> Sequential f b) -> Sequential f b
sequentialBind (Sequential a) f = Sequential $ a Base.>>= (getSequential . f)

-- INSTANCES

-- | Commutative `Monad`s form a duoid with themselves.
--
--  __NB__: Don’t use this newtype on a non-commutative Monad.
type Commutative :: forall {k}. (k -> Type) -> k -> Type
newtype Commutative f a = Commutative {getCommutative :: f a}
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

instance (Applicative f) => Applicative (Parallel (Commutative f)) where
  pure = Parallel . Commutative . Base.pure
  liftA2 f (Parallel (Commutative a)) (Parallel (Commutative b)) =
    Parallel . Commutative $ Base.liftA2 f a b

instance (Applicative f) => Applicative (Sequential (Commutative f)) where
  pure = Sequential . Commutative . Base.pure
  liftA2 f (Sequential (Commutative a)) (Sequential (Commutative b)) =
    Sequential . Commutative $ Base.liftA2 f a b

instance (Monad f) => Monad (Sequential (Commutative f)) where
  Sequential (Commutative a) >>= f =
    Sequential . Commutative $ a Base.>>= (getCommutative . getSequential . f)

-- Either

instance (Semigroup e) => Applicative (Parallel (Either e)) where
  pure = normalPure

  liftA2 f (Parallel a) (Parallel b) =
    Parallel $ case (a, b) of
      (Left e, Left e') -> Left $ e <> e'
      (Left e, Right _) -> Left e
      (Right _, Left e') -> Left e'
      (Right x, Right y) -> Right $ f x y

instance (Semigroup e) => Applicative (Sequential (Either e)) where
  pure = sequentialPure
  liftA2 = sequentialLiftA2

instance (Semigroup e) => Monad (Sequential (Either e)) where
  (>>=) = sequentialBind

instance (Semigroup e) => Normal (Either e)

-- | `Parallel` `Alternative` for `Either` collects failures unless there is a
--   success.
--
--  __TODO__: The unwrapped version of this should be added to @base@.
instance (Monoid e) => Alternative (Parallel (Either e)) where
  empty = Parallel $ Left mempty
  Parallel a <|> Parallel b = Parallel $ either (\e -> first (e <>) b) pure a

-- | `Sequential` `Alternative` for `Either` returns the last failure unless
--   there is a success.
instance (Monoid e) => Alternative (Sequential (Either e)) where
  empty = Sequential $ Left mempty
  Sequential a <|> Sequential b = Sequential $ either (const b) pure a

-- IO

instance Applicative (Parallel IO) where
  pure = normalPure
  liftA2 f (Parallel left) (Parallel right) =
    Parallel $ Async.liftA2 f left right

instance Applicative (Sequential IO) where
  pure = sequentialPure
  liftA2 = sequentialLiftA2

instance Monad (Sequential IO) where
  (>>=) = sequentialBind

instance Normal IO

-- | `Parallel` `Alternative` for `IO` returns the first action that completes,
--   canceling others. `empty` waits indefinitely.
instance Alternative (Parallel IO) where
  empty = Parallel Async.empty
  Parallel a <|> Parallel b = Parallel $ a Async.<|> b

instance Alternative (Sequential IO) where
  empty = Sequential empty
  Sequential a <|> Sequential b = Sequential $ a <|> b

-- tuples

-- |
--
--  __TODO__: For this one, we really _do_ need the separate `Sequential`,
--            because the `Monad` on @((,) a)@ only has a `Monoid` constraint,
--            and I don’t think we want to bless one of the `Duoid` monoids as
--            we currently do with the `Duoidal` functors.
instance (Duoid a) => Applicative (Parallel ((,) a)) where
  pure = Parallel . (pempty,)
  liftA2 f (Parallel (a, x)) (Parallel (a', y)) = Parallel (a |-| a', f x y)

instance (Duoid a) => Applicative (Sequential ((,) a)) where
  pure = Sequential . (sempty,)
  liftA2 = Base.liftM2

-- |
--
--  __NB__: This instance is different from the @`Monad` ((,) a)@ instance,
--          because it requires a `Duoid` with a /potentially/ distinct parallel
--          operation.
instance (Duoid a) => Monad (Sequential ((,) a)) where
  Sequential (u, a) >>= k =
    case k a of Sequential (v, b) -> Sequential (u >-> v, b)

-- | A writer is a `Normal` `Duoidal` functor when the writee is a
--   `Duoid.Normal` `Duoid`.
instance (Duoid.Normal a) => Normal ((,) a)
