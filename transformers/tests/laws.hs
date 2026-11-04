{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2026 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Main
  ( main,
  )
where

import safe "base" Control.Applicative ((<*>))
import safe "base" Control.Category ((.))
import safe "base" Data.Either (Either)
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.String (String)
import safe "base" Numeric.Natural (Natural)
import safe "base" System.IO (IO)
import safe "duoidal-transformers" Control.Duoidal.Trans.Orphans ()
import safe "duoids-hedgehog" Test.Duoidal qualified as Duoidal
import "hedgehog" Hedgehog qualified
import "hedgehog" Hedgehog.Gen qualified as Gen
import safe "hedgehog" Hedgehog.Main qualified as Hedgehog
import "hedgehog" Hedgehog.Range qualified as Range
import safe "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import safe "transformers" Control.Monad.Trans.Writer (WriterT (WriterT))

-- | Never empty, since an empty error can hide that two were combined.
genString :: Hedgehog.Gen String
genString = Gen.string (Range.constant 1 2) $ Gen.element "ab"

-- | `Either` accumulates errors in parallel and stops at the first in
--   sequence, so an underlying action run the wrong way is visible. This uses
--   `Gen.either_`, because `Gen.either` makes `Left` rare as the size grows.
genEither :: Hedgehog.Gen a -> Hedgehog.Gen (Either String a)
genEither = Gen.either_ genString

genExceptT :: Hedgehog.Gen a -> Hedgehog.Gen (ExceptT String (Either String) a)
genExceptT = fmap ExceptT . genEither . genEither

genWriterT :: Hedgehog.Gen a -> Hedgehog.Gen (WriterT Natural (Either String) a)
genWriterT genA =
  fmap WriterT . genEither $
    (,) <$> genA <*> Gen.integral (Range.linear 0 100)

-- | Checks the laws for transformers’ instances.
--
-- @since 99999
main :: IO ()
main =
  Hedgehog.defaultMain $
    fmap
      Hedgehog.checkParallel
      [ Duoidal.validateSequential genExceptT,
        Duoidal.validateSequential genWriterT
      ]
