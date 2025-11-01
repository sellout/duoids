{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:genGraph #-}

module Main (main) where

import safe "algebraic-graph-duoids" Algebra.Graph.Orphans.Duoid ()
import "algebraic-graphs" Algebra.Graph
  ( Graph (Connect, Empty, Overlay, Vertex),
  )
import safe "base" Control.Applicative (pure, (<*>))
import safe "base" Control.Category ((.))
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Ord ((<=))
import safe "base" Data.Word (Word8)
import safe "base" System.IO (IO)
import safe "duoids-hedgehog" Test.Duoid qualified as Duoid
import "hedgehog" Hedgehog qualified
import "hedgehog" Hedgehog.Gen qualified as Gen
import safe "hedgehog" Hedgehog.Main qualified as Hedgehog
import "hedgehog" Hedgehog.Range qualified as Range

genGraph :: (Hedgehog.MonadGen m) => m a -> m (Graph a)
genGraph genA =
  Gen.recursive
    Gen.choice
    [ pure Empty,
      Vertex <$> genA
    ]
    [ Overlay <$> genGraph genA <*> genGraph genA,
      Connect <$> genGraph genA <*> genGraph genA
    ]

main :: IO ()
main =
  Hedgehog.defaultMain $
    fmap
      Hedgehog.checkParallel
      [ Duoid.validateNormal @(Graph Word8) (<=) . genGraph $
          Gen.word8 Range.linearBounded
      ]
