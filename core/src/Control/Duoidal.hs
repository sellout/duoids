{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-methods:many,some #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
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
--         either “ambiguous occurrence of …” or “couldn’t satisfy constraint
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
    filterM,
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
    filterM',
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
    sequentialAp,
    sequentialBind,
    sequentialLiftA2,
    sequentialPure,

    -- * duoids from commutative `Monad`s
    commutativeAp,
  )
where

import safe "base" Control.Applicative
  ( Alternative,
    Applicative,
    Const (Const),
    empty,
    (<|>),
  )
import safe "base" Control.Applicative qualified as Base
  ( liftA2,
    liftA3,
    pure,
    (*>),
    (<*),
    (<**>),
    (<*>),
  )
import safe "base" Control.Category (id, (.))
import safe "base" Control.Monad (Monad)
import safe "base" Control.Monad qualified as Base
  ( ap,
    filterM,
    forever,
    join,
    liftM2,
    mapM,
    return,
    unless,
    when,
    (<=<),
    (=<<),
    (>=>),
    (>>=),
  )
import safe "base" Data.Bifunctor (bimap, first)
import safe "base" Data.Bitraversable (Bitraversable)
import safe "base" Data.Bitraversable qualified as Base
  ( bimapM,
    bisequence,
    bisequenceA,
    bitraverse,
  )
import safe "base" Data.Bool (Bool)
import safe "base" Data.Complex (Complex)
import safe "base" Data.Either (Either (Left, Right), either)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Foldable (Foldable)
import safe "base" Data.Foldable qualified as Base (for_, mapM_, traverse_)
import safe "base" Data.Function (const, flip, ($))
import safe "base" Data.Functor (Functor, fmap)
import safe "base" Data.Functor.Identity (Identity)
import safe "base" Data.Kind (Constraint, Type)
import safe "base" Data.Maybe (Maybe)
import safe "base" Data.Monoid (Dual, Monoid, Sum, mempty)
import safe "base" Data.Monoid qualified as Monoid
import safe "base" Data.Ord (Down, Ord)
import safe "base" Data.Proxy (Proxy)
import safe "base" Data.Semigroup (Max, Min, Semigroup, (<>))
import safe "base" Data.Semigroup qualified as Semigroup
import safe "base" Data.Traversable (Traversable)
import safe "base" Data.Traversable qualified as Base (for, traverse)
import safe "base" Data.Tuple (Solo)
import safe "base" GHC.TypeError (ErrorMessage (Text), TypeError)
import safe "base" System.IO (IO)
import safe "base" Text.Read (Read)
import safe "base" Text.Show (Show)
import "newtype" Control.Newtype (Newtype, ala, ala', op, over, under)
import safe "this" Control.Duoidal.Async qualified as Async
import safe "this" Control.Monad.Commutative (Commutative (Commutative))
import safe "this" Data.Duoid (Duoid, pempty, sempty, (>->), (|-|))
import safe "this" Data.Duoid qualified as Duoid (Normal)
import safe "base" Prelude (error)

-- I don’t know why this isn’t handled by the `ignore` in hlint.nix.
{-# HLINT ignore "Use traverse_" #-}

-- $setup
-- >>> :seti -XApplicativeDo
-- >>> :seti -XQualifiedDo
-- >>> import "duoids" Control.Duoidal qualified as Duoidal

type Parallel :: forall {k}. (k -> Type) -> k -> Type
newtype Parallel f a = Parallel {getParallel :: f a}
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

type role Parallel representational nominal

instance Newtype (Parallel f a) (f a)

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

type role Sequential representational nominal

instance Newtype (Sequential f a) (f a)

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

-- | The `Parallel` version of `Base.pure`.
pure :: (Duoidal f) => a -> f a
pure = op Parallel . Base.pure

-- | The `Sequential` version of `Base.return`.
return :: (Duoidal f) => a -> f a
return = op Sequential . Base.return

-- | The `Parallel` version of `Base.liftA2`.
liftA2 :: (Duoidal f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f = under Parallel . Base.liftA2 f . Parallel

-- | The `Sequential` version of `Base.liftM2`.
liftM2 :: (Duoidal f) => (a -> b -> c) -> f a -> f b -> f c
liftM2 f = under Sequential . Base.liftA2 f . Sequential

-- | The `Parallel` version of `Base.liftA3`.
liftA3 :: (Duoidal f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a = under Parallel . Base.liftA3 f (Parallel a) . Parallel

-- | The `Sequential` version of `Base.liftM3`.
liftM3 :: (Duoidal f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftM3 f a = under Sequential . Base.liftA3 f (Sequential a) . Sequential

-- | The `Parallel` version of `Base.<*>`.
(<*>) :: (Duoidal f) => f (a -> b) -> f a -> f b
f <*> g = getParallel $ Parallel f Base.<*> Parallel g

infixl 4 <*>

-- | The `Sequential` version of `Base.ap`.
ap :: (Duoidal f) => f (a -> b) -> f a -> f b
ap = under Sequential . Base.ap . Sequential

-- | The `Parallel` version of `Base.<**>`.
(<**>) :: (Duoidal f) => f a -> f (a -> b) -> f b
f <**> g = getParallel $ Parallel f Base.<**> Parallel g

infixl 4 <**>

-- | The `Sequential` version of `Base.<**>`.
--
--  __NB__: This is missing from "Control.Monad", but added here to maintain the
--         pairwise definitions of `Parallel` and `Sequential` `Applicative`
--         operations.
(<<>>) :: (Duoidal f) => f a -> f (a -> b) -> f b
f <<>> g = getSequential $ Sequential f Base.<**> Sequential g

infixl 1 <<>>

-- | The `Parallel` version of `Base.*>`.
(*>) :: (Duoidal f) => f a -> f b -> f b
a *> b = getParallel $ Parallel a Base.*> Parallel b

infixl 4 *>

-- | The `Sequential` version of `Base.>>`.
(>>) :: (Duoidal f) => f a -> f b -> f b
a >> b = getSequential $ Sequential a Base.*> Sequential b

infixl 1 >>

-- | The `Parallel` version of `Base.<*`.
(<*) :: (Duoidal f) => f a -> f b -> f a
a <* b = getParallel $ Parallel a Base.<* Parallel b

infixl 4 <*

-- | The `Sequential` version of `Base.<<`.
--
--  __NB__: This is missing from "Control.Monad", but added here to maintain the
--         pairwise definitions of `Parallel` and `Sequential` `Applicative`
--         operations.
(<<) :: (Duoidal f) => f a -> f b -> f a
a << b = getSequential $ Sequential a Base.<* Sequential b

infixl 1 <<

-- | The `Parallel` version of `Base.bisequenceA`.
bisequenceA :: (Bitraversable t, Duoidal f) => t (f a) (f b) -> f (t a b)
bisequenceA = op Parallel . Base.bisequenceA . bimap Parallel Parallel

-- | The `Sequential` version of `Base.bisequence`.
--
--  __NB__: In "Data.Bitraversable", this is identical to `Base.bisequenceA`
--          (it’s not overconstrained to `Monad`), but here we give it
--          sequential semantics, to parallel the difference between `sequenceA`
--          and `sequence`.
bisequence :: (Bitraversable t, Duoidal f) => t (f a) (f b) -> f (t a b)
bisequence = op Sequential . Base.bisequence . bimap Sequential Sequential

-- | The `Parallel` version of `Base.bitraverse`.
bitraverse ::
  (Bitraversable t, Duoidal f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bitraverse f = ala' Parallel . Base.bitraverse $ Parallel . f

-- | The `Sequential` version of `Base.bimapM`.
--
--  __NB__: In "Data.Bitraversable", this is identical to `Base.bitraverse`
--          (it’s not overconstrained to `Monad`), but here we give it
--          sequential semantics, to parallel the difference between `sequenceA`
--          and `sequence`.
bimapM ::
  (Bitraversable t, Duoidal f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bimapM f = ala' Sequential . Base.bimapM $ Sequential . f

-- | The `Parallel` version of `Base.traverse`.
traverse :: (Traversable t, Duoidal f) => (a -> f b) -> t a -> f (t b)
traverse = ala' Parallel Base.traverse

-- | The `Sequential` version of `Base.mapM`.
mapM :: (Traversable t, Duoidal f) => (a -> f b) -> t a -> f (t b)
mapM = ala' Sequential Base.mapM

-- | The `Parallel` version of `Base.traverse_`.
traverse_ :: (Foldable t, Duoidal f) => (a -> f b) -> t a -> f ()
traverse_ = ala' Parallel Base.traverse_

-- | The `Sequential` version of `Base.mapM_`.
mapM_ :: (Foldable t, Duoidal f) => (a -> f b) -> t a -> f ()
mapM_ = ala' Sequential Base.mapM_

-- | The `Parallel` version of `Base.filterM`.
--
--  __NB__: The definition in base is a bit idiosyncratic – it’s suffixed with
--          an @M@, but only requires `Applicative`, so here we give it
--          `Parallel` semantics (because that is usually what you’d want with a
--          drop-in replacement). The `Sequential` version is named `filterM'`.
filterM :: (Duoidal m) => (a -> m Bool) -> [a] -> m [a]
filterM = ala' Parallel Base.filterM

-- | The `Sequential` version of `Base.filterM`.
--
--  __TODO__: Come up with a convention for cases like this where there’s no
--            existing `Monad`-constrained name, and the
--            `Applicative`-constrained name uses the @M@ suffix. We could
--            rename the `Parallel` version to `filterA` (but that means the
--            “drop-in replacement” would default to `Sequential` semantics).
filterM' :: (Duoidal m) => (a -> m Bool) -> [a] -> m [a]
filterM' = ala' Sequential Base.filterM

-- | The `Parallel` version of `Base.for`.
for :: (Traversable t, Duoidal f) => t a -> (a -> f b) -> f (t b)
for = flip . ala' Parallel $ flip Base.for

-- | The `Sequential` version of `Base.forM`.
forM :: (Traversable t, Duoidal f) => t a -> (a -> f b) -> f (t b)
forM = flip . ala' Sequential $ flip Base.for

-- | The `Parallel` version of `Base.for_`.
for_ :: (Foldable t, Duoidal f) => t a -> (a -> f b) -> f ()
for_ = flip . ala' Parallel $ flip Base.for_

-- | The `Sequential` version of `Base.forM_`.
forM_ :: (Foldable t, Duoidal f) => t a -> (a -> f b) -> f ()
forM_ = flip . ala' Sequential $ flip Base.for_

-- | The `Parallel` version of `Base.sequenceA`.
sequenceA :: (Traversable t, Duoidal f) => t (f a) -> f (t a)
sequenceA = ala Parallel Base.traverse

-- | The `Sequential` version of `Base.sequence`.
sequence :: (Traversable t, Duoidal f) => t (f a) -> f (t a)
sequence = ala Sequential Base.traverse

-- | The `Parallel` version of `Base.sequenceA_`.
sequenceA_ :: (Foldable t, Duoidal f) => t (f a) -> f ()
sequenceA_ = ala Parallel Base.traverse_

-- | The `Sequential` version of `Base.sequence_`.
sequence_ :: (Foldable t, Duoidal f) => t (f a) -> f ()
sequence_ = ala Sequential Base.traverse_

-- | The `Parallel` version of `Base.forever`.
forever :: (Duoidal f) => f a -> f b
forever = under Parallel Base.forever

-- | The `Sequential` version of `Base.forever`.
forever' :: (Duoidal f) => f a -> f b
forever' = under Sequential Base.forever

-- | The `Parallel` version of `Base.unless`.
unless :: (Duoidal f) => Bool -> f () -> f ()
unless = under Parallel . Base.unless

-- | The `Sequential` version of `Base.unless`.
unless' :: (Duoidal f) => Bool -> f () -> f ()
unless' = under Sequential . Base.unless

-- | The `Parallel` version of `Base.when`.
when :: (Duoidal f) => Bool -> f () -> f ()
when = under Parallel . Base.when

-- | The `Sequential` version of `Base.when`.
when' :: (Duoidal f) => Bool -> f () -> f ()
when' = under Sequential . Base.when

join :: (Duoidal f) => f (f a) -> f a
join = getSequential . Base.join . Sequential . fmap Sequential

(>>=) :: (Duoidal f) => f a -> (a -> f b) -> f b
a >>= f = getSequential $ Sequential a Base.>>= Sequential . f

infixr 1 >>=

(=<<) :: (Duoidal f) => (a -> f b) -> f a -> f b
(=<<) f = under Sequential (Sequential . f Base.=<<)

infixr 1 =<<

(>=>) :: (Duoidal f) => (a -> f b) -> (b -> f c) -> a -> f c
(>=>) f = ala' Sequential (Sequential . f Base.>=>)

infixr 1 >=>

(<=<) :: (Duoidal f) => (b -> f c) -> (a -> f b) -> a -> f c
(<=<) f = ala' Sequential (Sequential . f Base.<=<)

infixr 1 <=<

type DuoidalIO :: (Type -> Type) -> Constraint
class (Duoidal m) => DuoidalIO m where
  liftIO :: IO a -> m a

instance DuoidalIO IO where
  liftIO = id

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
{-# DEPRECATED sequentialLiftA2 "use ‘sequentialAp’ instead" #-}

sequentialAp ::
  (Monad f) => Sequential f (a -> b) -> Sequential f a -> Sequential f b
sequentialAp = over Sequential . Base.ap . getSequential

sequentialBind ::
  (Monad f) => Sequential f a -> (a -> Sequential f b) -> Sequential f b
sequentialBind (Sequential a) f = Sequential $ a Base.>>= (getSequential . f)

-- INSTANCES

commutativeAp ::
  (Monad f) =>
  Parallel f (a -> b) ->
  Parallel f a ->
  Parallel f b
commutativeAp = over Parallel . Base.ap . getParallel

instance (Monad f) => Applicative (Parallel (Commutative f)) where
  pure = normalPure
  (<*>) = commutativeAp

instance (Monad f) => Applicative (Sequential (Commutative f)) where
  pure = sequentialPure
  (<*>) = sequentialAp

instance (Monad f) => Monad (Sequential (Commutative f)) where
  (>>=) = sequentialBind

instance (Monad f) => Normal (Commutative f)

-- `Complex` is a commutative duoidal functor

deriving via (Commutative Complex) instance Applicative (Parallel Complex)

deriving via (Commutative Complex) instance Applicative (Sequential Complex)

deriving via (Commutative Complex) instance Monad (Sequential Complex)

deriving via (Commutative Complex) instance Normal Complex

-- `Down` is a commutative duoidal functor

deriving via (Commutative Down) instance Applicative (Parallel Down)

deriving via (Commutative Down) instance Applicative (Sequential Down)

deriving via (Commutative Down) instance Monad (Sequential Down)

deriving via (Commutative Down) instance Normal Down

-- `Dual` is a commutative duoidal functor

deriving via (Commutative Dual) instance Applicative (Parallel Dual)

deriving via (Commutative Dual) instance Applicative (Sequential Dual)

deriving via (Commutative Dual) instance Monad (Sequential Dual)

deriving via (Commutative Dual) instance Normal Dual

-- `Monoid.First` is a commutative duoidal functor

deriving via
  (Commutative Monoid.First)
  instance
    Applicative (Parallel Monoid.First)

deriving via
  (Commutative Monoid.First)
  instance
    Applicative (Sequential Monoid.First)

deriving via (Commutative Monoid.First) instance Monad (Sequential Monoid.First)

deriving via (Commutative Monoid.First) instance Normal Monoid.First

-- `Semigroup.First` is a commutative duoidal functor

deriving via
  (Commutative Semigroup.First)
  instance
    Applicative (Parallel Semigroup.First)

deriving via
  (Commutative Semigroup.First)
  instance
    Applicative (Sequential Semigroup.First)

deriving via
  (Commutative Semigroup.First)
  instance
    Monad (Sequential Semigroup.First)

deriving via (Commutative Semigroup.First) instance Normal Semigroup.First

-- `Identity` is a commutative duoidal functor

deriving via (Commutative Identity) instance Applicative (Parallel Identity)

deriving via (Commutative Identity) instance Applicative (Sequential Identity)

deriving via (Commutative Identity) instance Monad (Sequential Identity)

deriving via (Commutative Identity) instance Normal Identity

-- `Monoid.Last` is a commutative duoidal functor

deriving via
  (Commutative Monoid.Last)
  instance
    Applicative (Parallel Monoid.Last)

deriving via
  (Commutative Monoid.Last)
  instance
    Applicative (Sequential Monoid.Last)

deriving via (Commutative Monoid.Last) instance Monad (Sequential Monoid.Last)

deriving via (Commutative Monoid.Last) instance Normal Monoid.Last

-- `Semigroup.Last` is a commutative duoidal functor

deriving via
  (Commutative Semigroup.Last)
  instance
    Applicative (Parallel Semigroup.Last)

deriving via
  (Commutative Semigroup.Last)
  instance
    Applicative (Sequential Semigroup.Last)

deriving via
  (Commutative Semigroup.Last)
  instance
    Monad (Sequential Semigroup.Last)

deriving via (Commutative Semigroup.Last) instance Normal Semigroup.Last

-- `Max` is a commutative duoidal functor

deriving via (Commutative Max) instance Applicative (Parallel Max)

deriving via (Commutative Max) instance Applicative (Sequential Max)

deriving via (Commutative Max) instance Monad (Sequential Max)

deriving via (Commutative Max) instance Normal Max

-- `Maybe` is a commutative duoidal functor

deriving via (Commutative Maybe) instance Applicative (Parallel Maybe)

deriving via (Commutative Maybe) instance Applicative (Sequential Maybe)

deriving via (Commutative Maybe) instance Monad (Sequential Maybe)

deriving via (Commutative Maybe) instance Normal Maybe

-- `Min` is a commutative duoidal functor

deriving via (Commutative Min) instance Applicative (Parallel Min)

deriving via (Commutative Min) instance Applicative (Sequential Min)

deriving via (Commutative Min) instance Monad (Sequential Min)

deriving via (Commutative Min) instance Normal Min

-- `Monoid.Product` is a commutative duoidal functor

deriving via
  (Commutative Monoid.Product)
  instance
    Applicative (Parallel Monoid.Product)

deriving via
  (Commutative Monoid.Product)
  instance
    Applicative (Sequential Monoid.Product)

deriving via
  (Commutative Monoid.Product)
  instance
    Monad (Sequential Monoid.Product)

deriving via (Commutative Monoid.Product) instance Normal Monoid.Product

-- `Proxy` is a commutative duoidal functor

deriving via (Commutative Proxy) instance Applicative (Parallel Proxy)

deriving via (Commutative Proxy) instance Applicative (Sequential Proxy)

deriving via (Commutative Proxy) instance Monad (Sequential Proxy)

deriving via (Commutative Proxy) instance Normal Proxy

-- `Solo` is a commutative duoidal functor

deriving via (Commutative Solo) instance Applicative (Parallel Solo)

deriving via (Commutative Solo) instance Applicative (Sequential Solo)

deriving via (Commutative Solo) instance Monad (Sequential Solo)

deriving via (Commutative Solo) instance Normal Solo

-- `Sum` is a commutative duoidal functor

deriving via (Commutative Sum) instance Applicative (Parallel Sum)

deriving via (Commutative Sum) instance Applicative (Sequential Sum)

deriving via (Commutative Sum) instance Monad (Sequential Sum)

deriving via (Commutative Sum) instance Normal Sum

-- reader is a commutative duoidal functor

deriving via (Commutative ((->) r)) instance Applicative (Parallel ((->) r))

deriving via (Commutative ((->) r)) instance Applicative (Sequential ((->) r))

deriving via (Commutative ((->) r)) instance Monad (Sequential ((->) r))

deriving via (Commutative ((->) r)) instance Normal ((->) r)

-- Const

instance (Monoid a) => Applicative (Parallel (Const a)) where
  pure = normalPure
  liftA2 f (Parallel a) (Parallel b) = Parallel $ liftA2 f a b

instance (Monoid a) => Applicative (Sequential (Const a)) where
  pure = Sequential . pure
  liftA2 f (Sequential a) = Sequential . Base.liftA2 f a . getSequential

-- | The `Const` duoidal functor provides an illustration of why we need to have
--   both `Parallel` and `Sequential` newtypes – relying on the underlying
--   `Applicative` (and only having the `Sequential` newtype) would mean that
--   any duoidal structure would only have a `Monad` available under
--   `Sequential`, which would be a prettty serious impact. On the other hand,
--   relying on the underlying `Monad` (and only having the `Parallel` newtype)
--   is much more natural, but `Const`, for example, having a `Monad` instance
--   would make it basically useless, and the more interesting `Applicative`
--   instance would only be available under the `Parallel` netwype. The current
--   structure allows either the `Applicative` or `Monad` instance to be the one
--   exposed directly.
instance (Monoid a) => Monad (Sequential (Const a)) where
  Sequential (Const a) >>= _ = Sequential $ Const a

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
  (<*>) = sequentialAp

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
  (<*>) = sequentialAp

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

instance (Duoid a, Duoid b) => Applicative (Parallel ((,,) a b)) where
  pure = Parallel . (pempty,pempty,)
  liftA2 f (Parallel (a, b, x)) (Parallel (a', b', y)) =
    Parallel (a |-| a', b |-| b', f x y)

instance (Duoid a, Duoid b) => Applicative (Sequential ((,,) a b)) where
  pure = Sequential . (sempty,sempty,)
  liftA2 = Base.liftM2

instance (Duoid a, Duoid b) => Monad (Sequential ((,,) a b)) where
  Sequential (u, v, a) >>= k =
    case k a of Sequential (u', v', b) -> Sequential (u >-> u', v >-> v', b)

-- | A writer is a `Normal` `Duoidal` functor when the writee is a
--   `Duoid.Normal` `Duoid`.
instance (Duoid.Normal a, Duoid.Normal b) => Normal ((,,) a b)

instance
  (Duoid a, Duoid b, Duoid c) =>
  Applicative (Parallel ((,,,) a b c))
  where
  pure = Parallel . (pempty,pempty,pempty,)
  liftA2 f (Parallel (a, b, c, x)) (Parallel (a', b', c', y)) =
    Parallel (a |-| a', b |-| b', c |-| c', f x y)

instance
  (Duoid a, Duoid b, Duoid c) =>
  Applicative (Sequential ((,,,) a b c))
  where
  pure = Sequential . (sempty,sempty,sempty,)
  liftA2 = Base.liftM2

instance (Duoid a, Duoid b, Duoid c) => Monad (Sequential ((,,,) a b c)) where
  Sequential (u, v, w, a) >>= k =
    case k a of
      Sequential (u', v', w', b) -> Sequential (u >-> u', v >-> v', w >-> w', b)

-- | A writer is a `Normal` `Duoidal` functor when the writee is a
--   `Duoid.Normal` `Duoid`.
instance
  (Duoid.Normal a, Duoid.Normal b, Duoid.Normal c) =>
  Normal ((,,,) a b c)
