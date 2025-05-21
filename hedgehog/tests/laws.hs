{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Main (main) where

import safe "base" Control.Applicative (liftA2)
import safe "base" Control.Category ((.))
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap)
import safe "base" Data.Int (Int64)
import safe "base" Data.Ord ((<=))
import safe "base" Data.Ratio (Ratio, Rational, (%))
import safe "base" Data.Word (Word64)
import safe "base" Numeric.Natural (Natural)
import safe "base" System.IO (IO)
import safe "duoids-hedgehog" Test.Duoid qualified as Duoid
import "hedgehog" Hedgehog qualified
import "hedgehog" Hedgehog.Gen qualified as Gen
import safe "hedgehog" Hedgehog.Main qualified as Hedgehog
import "hedgehog" Hedgehog.Range qualified as Range
import safe "base" Prelude (Bounded, Integral, fromIntegral, maxBound, minBound, (+), (-))

-- prop_natural_normalDuoid :: Hedgehog.Property
-- prop_natural_normalDuoid =
--   Duoid.validateNormal
--     (<=)'
--     (Gen.integral @_ @Natural (Range.linearFrom 0 0 (1 + fromIntegral (maxBound :: Word64))))

-- prop_word_normalDuoid :: Hedgehog.Property
-- prop_word_normalDuoid = Duoid.validateNormal (<=) (Gen.word Range.linearBounded)

-- prop_word16_normalDuoid :: Hedgehog.Property
-- prop_word16_normalDuoid =
--   Duoid.validateNormal (<=) (Gen.word16 Range.linearBounded)

-- prop_word32_normalDuoid :: Hedgehog.Property
-- prop_word32_normalDuoid =
--   Duoid.validateNormal (<=) (Gen.word32 Range.linearBounded)

-- prop_word64_normalDuoid :: Hedgehog.Property
-- prop_word64_normalDuoid =

-- prop_word8_normalDuoid :: Hedgehog.Property
-- prop_word8_normalDuoid =
--   Duoid.validateNormal (<=) (Gen.word8 Range.linearBounded)

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

-- |
--
--  __FIXME__: For some reason, the `Ratio` cases generate (or shrink to) “Ratio
--             has zero denominator”.
main :: IO ()
main =
  Hedgehog.defaultMain $
    fmap
      Hedgehog.checkParallel
      [ Duoid.validate (<=) (Gen.int Range.linearBounded),
        Duoid.validate (<=) (Gen.int16 Range.linearBounded),
        Duoid.validate (<=) (Gen.int32 Range.linearBounded),
        Duoid.validate (<=) (Gen.int64 Range.linearBounded),
        Duoid.validate (<=) (Gen.int8 Range.linearBounded),
        Duoid.validateNormal (<=)
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
        Duoid.validateNormal (<=) $ Gen.word8 Range.linearBounded
      ]
