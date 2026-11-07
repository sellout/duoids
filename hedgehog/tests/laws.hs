{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
#if !MIN_VERSION_GLASGOW_HASKELL(9, 8, 0, 0)
-- Older GHC reports redundant constraints when a passed function doesn’t use
-- the allowed constraints, as is the cas for Proxy’s laws.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
module Main (main) where

import safe "base" Control.Applicative (liftA2, pure, (<*>))
import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool, (&&))
import safe "base" Data.Complex (Complex ((:+)))
import safe "base" Data.Either (Either)
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Functor.Classes (liftEq2)
import safe "base" Data.Functor.Identity (Identity (Identity))
import safe "base" Data.Int (Int64)
import safe "base" Data.List (isInfixOf)
import safe "base" Data.Monoid qualified as Monoid
import safe "base" Data.Ord (Down (Down), (<=))
import safe "base" Data.Proxy (Proxy (Proxy))
import safe "base" Data.Ratio (Ratio, Rational, (%))
import safe "base" Data.Semigroup qualified as Semigroup
import safe "base" Data.String (String)
import safe "base" Data.Tuple (Solo)
import safe "base" Data.Word (Word64)
import safe "base" Numeric.Natural (Natural)
import safe "base" System.IO (IO)
import safe "duoids" Control.Duoidal qualified as Duoidal (Commutative (Commutative))
import safe "duoids-hedgehog" Test.Duoid qualified as Duoid
import safe "duoids-hedgehog" Test.Duoidal qualified as Duoidal
import "hedgehog" Hedgehog qualified
import "hedgehog" Hedgehog.Gen qualified as Gen
import safe "hedgehog" Hedgehog.Main qualified as Hedgehog
import "hedgehog" Hedgehog.Range qualified as Range
import safe "base" Prelude (Bounded, Integral, fromIntegral, maxBound, minBound, (+), (-))

_genRealFrac :: forall a. (Bounded a, Integral a) => Hedgehog.Gen (Ratio a)
_genRealFrac =
  -- TODO: Was hoping for an implementation like this, but it errors (during
  --       shrinking?) with a zero denominator.
  -- Gen.realFrac_
  --   . Range.linearFracFrom 0 (fromIntegral $ minBound @a)
  --   . fromIntegral
  --   $ maxBound @a
  liftA2 (%) (Gen.integral . Range.linearFrom 0 (minBound @a) $ maxBound @a)
    . Gen.integral
    . Range.linear 1
    $ maxBound @a

-- | Never empty, since it can obscure how combinations were reached.
genString :: Hedgehog.Gen String
genString = Gen.string (Range.constant 1 2) $ Gen.element "ab"

-- |
--
--  __NOTE__: This uses `Gen.either_`, because `Gen.either` makes `Left` rare as
--            the size grows.
genEither :: Hedgehog.Gen a -> Hedgehog.Gen (Either String a)
genEither = Gen.either_ genString

genNatural :: Hedgehog.Gen Natural
genNatural = Gen.integral $ Range.linear 0 100

genWriter :: Hedgehog.Gen a -> Hedgehog.Gen (Natural, a)
genWriter = liftA2 (,) genNatural

-- | `Rational`’s units differ, so this writer isn’t normal.
genRationalWriter :: Hedgehog.Gen a -> Hedgehog.Gen (Rational, a)
genRationalWriter =
  liftA2 (,) . Gen.realFrac_ $ Range.linearFracFrom 0 (-100) 100

genWriter3 :: Hedgehog.Gen a -> Hedgehog.Gen (Natural, Natural, a)
genWriter3 genA = (,,) <$> genNatural <*> genNatural <*> genA

genWriter4 :: Hedgehog.Gen a -> Hedgehog.Gen (Natural, Natural, Natural, a)
genWriter4 genA = (,,,) <$> genNatural <*> genNatural <*> genNatural <*> genA

-- | Writers relate by their duoids’ order on each output.
relWriter3 :: (Eq x) => (Natural, Natural, x) -> (Natural, Natural, x) -> Bool
relWriter3 (a, b, x) (a', b', y) = a <= a' && b <= b' && x == y

relWriter4 ::
  (Eq x) => (Natural, Natural, Natural, x) -> (Natural, Natural, Natural, x) -> Bool
relWriter4 (a, b, c, x) (a', b', c', y) =
  a <= a' && b <= b' && c <= c' && x == y

-- | Checks the duoid laws for types in base, and the duoidal laws for functors
--   in base and this package. `IO` and @(->) r@ have no `Data.Eq.Eq`, so
--   their `Duoidal` instances go unchecked.
--
--  __FIXME__: For some reason, the `Ratio` cases generate (or shrink to) “Ratio
--             has zero denominator”.
--
-- @since 0.0.1
main :: IO ()
main =
  Hedgehog.defaultMain $
    fmap
      Hedgehog.checkParallel
      [ Duoid.validateNormal (<=)
          . Gen.integral @_ @Natural
          . Range.linearFrom 0 0
          -- The range bound here pushes it just outside the range of
          -- `Bounded` integers.
          $ fromIntegral (maxBound :: Word64) + 1,
        -- Duoid.validate @(Ratio Int) (<=) genRealFrac,
        -- Duoid.validate @(Ratio Int16) (<=) genRealFrac,
        -- Duoid.validate @(Ratio Int32) (<=) genRealFrac,
        -- Duoid.validate @(Ratio Int64) (<=) genRealFrac,
        -- Duoid.validate @(Ratio Int8) (<=) genRealFrac,
        Duoid.validateNormal @(Ratio Natural) (<=)
          . Gen.realFrac_
          -- The range bounds here push it just outside the range of
          -- `Bounded` integers.
          . Range.linearFracFrom 0 0
          $ fromIntegral (maxBound :: Word64) + 1,
        -- Duoid.validateNormal @(Ratio Word) (<=) genRealFrac,
        -- Duoid.validateNormal @(Ratio Word16) (<=) genRealFrac,
        -- Duoid.validateNormal @(Ratio Word32) (<=) genRealFrac,
        -- Duoid.validateNormal @(Ratio Word64) (<=) genRealFrac,
        -- Duoid.validateNormal @(Ratio Word8) (<=) genRealFrac,
        Duoid.validate @Rational (<=)
          . Gen.realFrac_
          -- The range bounds here push it just outside the range of
          -- `Bounded` integers.
          . Range.linearFracFrom 0 (fromIntegral (minBound :: Int64) - 1)
          $ fromIntegral (maxBound :: Word64) + 1,
        Duoid.validateNormal (<=) $ Gen.word Range.linearBounded,
        Duoid.validateNormal (<=) $ Gen.word16 Range.linearBounded,
        Duoid.validateNormal (<=) $ Gen.word32 Range.linearBounded,
        Duoid.validateNormal (<=) $ Gen.word64 Range.linearBounded,
        Duoid.validateNormal (<=) $ Gen.word8 Range.linearBounded,
        -- Sequencing stops at the first error, so interchange can drop some.
        Duoidal.validateNormal (liftEq2 (flip isInfixOf) (==)) genEither,
        Duoidal.validateNormal (==) Gen.maybe,
        Duoidal.validateNormal (==) $ fmap Identity,
        Duoidal.validateNormal (==) $ fmap Duoidal.Commutative . Gen.maybe,
        Duoidal.validateNormal (==) $ \genA -> (:+) <$> genA <*> genA,
        Duoidal.validateNormal (==) $ fmap Down,
        Duoidal.validateNormal (==) $ fmap Monoid.Dual,
        Duoidal.validateNormal (==) $ fmap Monoid.First . Gen.maybe,
        Duoidal.validateNormal (==) $ fmap Monoid.Last . Gen.maybe,
        Duoidal.validateNormal (==) $ fmap Monoid.Product,
        Duoidal.validateNormal (==) $ fmap Monoid.Sum,
        Duoidal.validateNormal (==) $ fmap Semigroup.First,
        Duoidal.validateNormal (==) $ fmap Semigroup.Last,
        Duoidal.validateNormal (==) $ fmap Semigroup.Max,
        Duoidal.validateNormal (==) $ fmap Semigroup.Min,
        Duoidal.validateNormal (==) $ \_ -> pure Proxy,
        Duoidal.validateNormal @Solo (==) $ fmap pure,
        -- Output combines by `max` in parallel and `+` in sequence, and a
        -- maximum of sums is at most the sum of maxima.
        Duoidal.validateNormal (liftEq2 (<=) (==)) genWriter,
        Duoidal.validate (liftEq2 (<=) (==)) genRationalWriter,
        Duoidal.validateNormal relWriter3 genWriter3,
        Duoidal.validateNormal relWriter4 genWriter4
      ]
