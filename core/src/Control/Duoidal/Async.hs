{-# LANGUAGE Safe #-}

-- |
-- Copyright:
--   2012 Simon Marlow
--   2024 Greg Pfeil
-- License: BSD-3-Clause
-- Stability: provisional
-- Portability: non-portable (requires concurrency)
--
-- This largely contains code copied from
-- "async:Control.Concurrent.Async.Internal", and thus inherits the copyright
-- from that module.
--
-- It’s copied rather than depended on to avoid the dependencies on
-- [hashable](https://hackage.haskell.org/package/hashable) and
-- [stm](https://hackage.haskell.org/package/stm).
module Control.Duoidal.Async
  ( liftA2,
    empty,
    (<|>),
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category (id, (.))
import "base" Control.Concurrent
  ( forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
  )
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
import "base" Control.Monad (forever, replicateM_, return, when, (=<<), (>>=))
import "base" Data.Either (Either (Left, Right), either)
import "base" Data.Eq (Eq)
import "base" Data.Function (($))
import "base" Data.Functor (void, (<$>))
import "base" Data.IORef (modifyIORef, newIORef, readIORef)
import "base" Data.Int (Int)
import "base" Data.Kind (Type)
import "base" Data.Ord ((>))
import "base" System.IO (IO)
import "base" Text.Show (Show)
import "base" Prelude (maxBound, subtract)

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
      forkIO
        . uninterruptibleMask_
        $ restore (left >>= putMVar done . Right . Left)
          `catch` (putMVar done . Left)
    rid <-
      forkIO
        . uninterruptibleMask_
        $ restore (right >>= putMVar done . Right . Right)
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

-- | Run two @IO@ actions concurrently, and return the first to
-- finish.  The loser of the race is 'cancel'led.
--
-- > race left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitEither a b
race :: IO a -> IO b -> IO (Either a b)
race left right = concurrently' left right (either throwIO return =<<)

liftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c
liftA2 f left right = concurrently' left right (collect [])
  where
    collect [Left a, Right b] _ = pure $ f a b
    collect [Right b, Left a] _ = pure $ f a b
    collect xs m = do
      e <- m
      case e of
        Left ex -> throwIO ex
        Right r -> collect (r : xs) m

empty :: IO a
empty = forever $ threadDelay maxBound

(<|>) :: IO a -> IO a -> IO a
a <|> b = either id id <$> race a b

infixl 3 <|>
