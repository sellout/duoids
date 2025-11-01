{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-methods:sconcat #-}

module Algebra.Graph.Orphans.Duoid () where

import "algebraic-graphs" Algebra.Graph (Graph (Connect, Empty, Overlay))
import safe "base" Data.Function (($))
import safe "base" Data.Monoid (Monoid, mempty)
import safe "base" Data.Semigroup (Semigroup, (<>))
import safe "duoids" Data.Duoid (Duoid, Par (Par), Seq (Seq))
import safe "duoids" Data.Duoid qualified as Duoid (Normal)

instance Semigroup (Par (Graph a)) where
  Par x <> Par y = Par $ Overlay x y

instance Monoid (Par (Graph a)) where
  mempty = Par Empty

instance Semigroup (Seq (Graph a)) where
  Seq x <> Seq y = Seq $ Connect x y

instance Monoid (Seq (Graph a)) where
  mempty = Seq Empty

instance Duoid (Graph a)

instance Duoid.Normal (Graph a)
