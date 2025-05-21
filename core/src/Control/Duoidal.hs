{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides duoidal operations on functors. This lets us easily mix and match
--   "parallel" and "sequential" operations on structures that have multiple
--   viable `Applicative` instances, like `Either`
--   (`Data.Either.Validation.Validation`) and `System.IO.IO`
--   (`Control.Concurrent.Async.Concurrently`).
--
--   So, for example, when using this, you should ignore the existence of
--   `Data.Either.Validation.Validation`, and always work in `Either` (and
--   `ExceptT`), then, using these operators instead of the usual `Applicative`
--   and `Monad` operators, you will have behavior that correctly mixes the
--   accumulation of errors with the monadic "first failure" semantics. This
--   should lawfully always do what you want, without running into the
--   “`Applicative` semantics must match `Monad` semantics” problem.
--
-- * ♢ – `Parallel`
-- * ★ – `Sequential`
--
--   Resources:
--
-- * https://ncatlab.org/nlab/show/duoidal+category
-- * https://blogs.ncl.ac.uk/andreymokhov/united-monoids/
module Control.Duoidal
  ( Duoidal,
    Parallel (..),
    Sequential (..),
    (<*\>),
    (<=\<),
    (>>\=),
    (=<\<),
    (>\>),
    (<\*),
    (*\>),
    bisequenceD,
    bitraverseD,
    joinD,
    liftD2,
    pureD,
    sequenceD,
    traverseD,
  )
where

import "base" Control.Applicative (Applicative, liftA2, pure, (*>), (<*), (<*>))
import "base" Control.Category ((.))
import "base" Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import "base" Control.Exception
  ( BlockedIndefinitelyOnMVar (BlockedIndefinitelyOnMVar),
    Exception,
    SomeException,
    asyncExceptionFromException,
    asyncExceptionToException,
    catch,
    fromException,
    mask,
    onException,
    throwIO,
    throwTo,
    toException,
    uninterruptibleMask_,
  )
import "base" Control.Monad (Monad, join, replicateM_, when, (<=<), (=<<), (>>), (>>=))
import "base" Data.Bifunctor (bimap)
import "base" Data.Bitraversable (Bitraversable, bisequence, bitraverse)
import "base" Data.Either (Either (Left, Right))
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Functor (Functor, fmap, void)
import "base" Data.IORef (modifyIORef, newIORef, readIORef)
import "base" Data.Int (Int)
import "base" Data.Kind (Constraint, Type)
import "base" Data.Ord ((>))
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.Traversable (Traversable, traverse)
import "base" System.IO (IO)
import "base" Text.Show (Show)
import "base" Prelude (subtract)

type Parallel :: forall {k}. (k -> Type) -> k -> Type
newtype Parallel f a = Parallel {getParallel :: f a}

type Sequential :: forall {k}. (k -> Type) -> k -> Type
newtype Sequential f a = Sequential {getSequential :: f a}

instance (Functor f) => Functor (Parallel f) where
  fmap f = Parallel . fmap f . getParallel

instance (Functor f) => Functor (Sequential f) where
  fmap f = Sequential . fmap f . getSequential

-- | A `Duoidal` functor is a `Monad`, but is also an `Applicative` functor in
--   two ways. One is the `Applicative` induced by the `Monad`, but the other is
--   a “`Parallel`” `Applicative`, which relates to the monadic (“`Sequential`”)
--   `Applicative` via the duoid laws.
type Duoidal :: (Type -> Type) -> Constraint
class (Functor f, Applicative (Parallel f), Monad (Sequential f)) => Duoidal f

-- interchange :: f a -> f a
-- splitUnit :: f a -> f (a, a)
-- joinUnit :: f (a, a) -> f a
-- swapUnit :: f a -> f a
-- swapUnit = defaultSwapUnit

-- I→≅(J⋆I)⋄(I⋆J)→(J⋄I)⋆(I⋄J)→≅J

-- defaultSwapUnit :: (Duoidal f) => f a -> f a
-- defaultSwapUnit =
--   to rightIdentity --                                           J
--     . bimap (to rightIdentity) (to leftIdentity) --           J ★ J
--     . interchange --                                    (J ♢ I) ★ (I ♢ J)
--     . bimap (from leftIdentity) (from rightIdentity) -- (J ★ I) ♢ (I ★ J)
--     . from leftIdentity --                                    I ♢ I

-- -- | Normal duoids allow us to convert from the `Sequential` `pure` to the
-- --  `Parallel` `pure`. Most of our Duoids are likely normal, as the `pure` tends
-- --   to be the same for both.
-- class Duoidal f => Normal f where
--   -- | This must form an isomorphism with `swapUnit`.
--   unswapUnit :: f a -> f a

pureD :: (Duoidal f) => a -> f a
pureD = getParallel . pure

liftD2 :: (Duoidal f) => (a -> b -> c) -> f a -> f b -> f c
liftD2 f a b = getParallel (liftA2 f (Parallel a) (Parallel b))

(<*\>) :: (Duoidal f) => f (a -> b) -> f a -> f b
f <*\> g = getParallel (Parallel f <*> Parallel g)

infixl 4 <*\>

joinD :: (Duoidal f) => f (f a) -> f a
joinD = getSequential . join . Sequential . fmap Sequential

(>>\=) :: (Duoidal f) => f a -> (a -> f b) -> f b
a >>\= f = getSequential (Sequential . f =<< Sequential a)

infixr 1 >>\=

(=<\<) :: (Duoidal f) => (a -> f b) -> f a -> f b
f =<\< a = getSequential (Sequential . f =<< Sequential a)

infixr 1 =<\<

(<=\<) :: (Duoidal f) => (b -> f c) -> (a -> f b) -> a -> f c
f <=\< g = getSequential . (Sequential . f <=< Sequential . g)

infixr 1 <=\<

(>\>) :: (Duoidal f) => f a -> f b -> f b
a >\> b = getSequential (Sequential a >> Sequential b)

(*\>) :: (Duoidal f) => f a -> f b -> f b
a *\> b = getParallel (Parallel a *> Parallel b)

(<\*) :: (Duoidal f) => f a -> f b -> f a
a <\* b = getParallel (Parallel a <* Parallel b)

-- | `bisequence` over a `Duoidal`.
bisequenceD :: (Bitraversable t, Duoidal f) => t (f a) (f b) -> f (t a b)
bisequenceD = getParallel . bisequence . bimap Parallel Parallel

-- | `bitraverse` over a `Duoidal`.
bitraverseD :: (Bitraversable t, Duoidal f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
bitraverseD f g = getParallel . bitraverse (Parallel . f) (Parallel . g)

-- | `sequenceA` over a `Duoidal`.
sequenceD :: (Traversable t, Duoidal f) => t (f a) -> f (t a)
sequenceD = getParallel . traverse Parallel

-- | `traverse` over a `Duoidal`.
traverseD :: (Traversable t, Duoidal f) => (a -> f b) -> t a -> f (t b)
traverseD f = getParallel . traverse (Parallel . f)

-- INSTANCES

instance (Semigroup e) => Applicative (Parallel (Either e)) where
  pure = Parallel . pure

  liftA2 f (Parallel a) (Parallel b) =
    Parallel $ case (a, b) of
      (Left e, Left e') -> Left $ e <> e'
      (Left e, Right _) -> Left e
      (Right _, Left e') -> Left e'
      (Right x, Right y) -> Right $ f x y

instance (Semigroup e) => Applicative (Sequential (Either e)) where
  pure = Sequential . pure

  liftA2 f (Sequential a) (Sequential b) = Sequential (liftA2 f a b)

instance (Semigroup e) => Monad (Sequential (Either e)) where
  Sequential a >>= f = Sequential (a >>= getSequential . f)

instance (Semigroup e) => Duoidal (Either e)

instance Applicative (Parallel IO) where
  pure = Parallel . pure

  liftA2 f (Parallel left) (Parallel right) =
    Parallel $ concurrently' left right (collect [])
    where
      collect [Left a, Right b] _ = pure $ f a b
      collect [Right b, Left a] _ = pure $ f a b
      collect xs m = do
        e <- m
        case e of
          Left ex -> throwIO ex
          Right r -> collect (r : xs) m

instance Applicative (Sequential IO) where
  pure = Sequential . pure

  liftA2 f (Sequential a) (Sequential b) = Sequential (liftA2 f a b)

instance Monad (Sequential IO) where
  Sequential a >>= f = Sequential (a >>= getSequential . f)

instance Duoidal IO

concurrently' ::
  IO a ->
  IO b ->
  (IO (Either SomeException (Either a b)) -> IO r) ->
  IO r
concurrently' left right collect = do
  done <- newEmptyMVar
  mask $ \restore -> do
    -- Note: uninterruptibleMask here is because we must not allow
    -- the putMVar in the exception handler to be interrupted,
    -- otherwise the parent thread will deadlock when it waits for
    -- the thread to terminate.
    lid <-
      forkIO . uninterruptibleMask_ $
        restore (left >>= putMVar done . Right . Left)
          `catch` (putMVar done . Left)
    rid <-
      forkIO . uninterruptibleMask_ $
        restore (right >>= putMVar done . Right . Right)
          `catch` (putMVar done . Left)

    count <- newIORef (2 :: Int)
    let takeDone = do
          r <- takeMVar done -- interruptible
          -- Decrement the counter so we know how many takes are left.
          -- Since only the parent thread is calling this, we can
          -- use non-atomic modifications.
          -- NB. do this *after* takeMVar, because takeMVar might be
          -- interrupted.
          modifyIORef count (subtract 1)
          pure r

    let tryAgain f = f `catch` \BlockedIndefinitelyOnMVar -> f

        stop = do
          -- kill right before left, to match the semantics of
          -- the version using withAsync. (#27)
          uninterruptibleMask_ $ do
            count' <- readIORef count
            -- we only need to use killThread if there are still
            -- children alive.  Note: forkIO here is because the
            -- child thread could be in an uninterruptible
            -- putMVar.
            when (count' > 0) . void . forkIO $ do
              throwTo rid AsyncCancelled
              throwTo lid AsyncCancelled
            -- ensure the children are really dead
            replicateM_ count' (tryAgain $ takeMVar done)

    r <- collect (tryAgain takeDone) `onException` stop
    stop
    pure r

-- | The exception thrown by `cancel` to terminate a thread.
type AsyncCancelled :: Type
data AsyncCancelled = AsyncCancelled
  deriving stock (Show, Eq)

instance Exception AsyncCancelled where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException
