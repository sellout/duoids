{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Algebra.Graph.Duoid.Orphans () where

import "algebraic-graphs" Algebra.Graph qualified as Unlabeled (Graph)
import "algebraic-graphs" Algebra.Graph.AdjacencyIntMap (AdjacencyIntMap)
import "algebraic-graphs" Algebra.Graph.AdjacencyMap qualified as Unlabeled
  ( AdjacencyMap,
  )
import "algebraic-graphs" Algebra.Graph.Example.Todo (Todo)
import "algebraic-graphs" Algebra.Graph.Label (Dioid)
import "algebraic-graphs" Algebra.Graph.Labelled qualified as Labeled (Graph)
import "algebraic-graphs" Algebra.Graph.Labelled.AdjacencyMap qualified as Labeled
  ( AdjacencyMap,
  )
import "algebraic-graphs" Algebra.Graph.Relation (Relation)
import "algebraic-graphs" Algebra.Graph.Relation.Preorder (PreorderRelation)
import "algebraic-graphs" Algebra.Graph.Relation.Reflexive (ReflexiveRelation)
import "algebraic-graphs" Algebra.Graph.Relation.Symmetric qualified as Symmetric
  ( Relation,
  )
import "algebraic-graphs" Algebra.Graph.Relation.Transitive (TransitiveRelation)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Monoid (Monoid, mempty)
import safe "base" Data.Ord (Ord)
import safe "base" Data.Semigroup (Semigroup, (<>))
import safe "duoids" Data.Duoid (Par, Seq)
import safe "duoids" Data.Duoid qualified as Duoid (Normal)
import safe "this" Algebra.Graph.Duoid
  ( parEmptyGraph,
    parGraph,
    seqEmptyGraph,
    seqGraph,
  )

-- adjacency int map

instance Semigroup (Par AdjacencyIntMap) where
  (<>) = parGraph

instance Monoid (Par AdjacencyIntMap) where
  mempty = parEmptyGraph

instance Semigroup (Seq AdjacencyIntMap) where
  (<>) = seqGraph

instance Monoid (Seq AdjacencyIntMap) where
  mempty = seqEmptyGraph

instance Duoid.Normal AdjacencyIntMap

-- unlabeled adjacency map

instance (Ord a) => Semigroup (Par (Unlabeled.AdjacencyMap a)) where
  (<>) = parGraph

instance (Ord a) => Monoid (Par (Unlabeled.AdjacencyMap a)) where
  mempty = parEmptyGraph

instance (Ord a) => Semigroup (Seq (Unlabeled.AdjacencyMap a)) where
  (<>) = seqGraph

instance (Ord a) => Monoid (Seq (Unlabeled.AdjacencyMap a)) where
  mempty = seqEmptyGraph

instance (Ord a) => Duoid.Normal (Unlabeled.AdjacencyMap a)

-- relation

instance (Ord a) => Semigroup (Par (Relation a)) where
  (<>) = parGraph

instance (Ord a) => Monoid (Par (Relation a)) where
  mempty = parEmptyGraph

instance (Ord a) => Semigroup (Seq (Relation a)) where
  (<>) = seqGraph

instance (Ord a) => Monoid (Seq (Relation a)) where
  mempty = seqEmptyGraph

instance (Ord a) => Duoid.Normal (Relation a)

-- symmetric relation

instance (Ord a) => Semigroup (Par (Symmetric.Relation a)) where
  (<>) = parGraph

instance (Ord a) => Monoid (Par (Symmetric.Relation a)) where
  mempty = parEmptyGraph

instance (Ord a) => Semigroup (Seq (Symmetric.Relation a)) where
  (<>) = seqGraph

instance (Ord a) => Monoid (Seq (Symmetric.Relation a)) where
  mempty = seqEmptyGraph

instance (Ord a) => Duoid.Normal (Symmetric.Relation a)

-- unlabeled graph

instance Semigroup (Par (Unlabeled.Graph a)) where
  (<>) = parGraph

instance Monoid (Par (Unlabeled.Graph a)) where
  mempty = parEmptyGraph

instance Semigroup (Seq (Unlabeled.Graph a)) where
  (<>) = seqGraph

instance Monoid (Seq (Unlabeled.Graph a)) where
  mempty = seqEmptyGraph

instance Duoid.Normal (Unlabeled.Graph a)

-- transitive relation

instance (Ord a) => Semigroup (Par (TransitiveRelation a)) where
  (<>) = parGraph

instance (Ord a) => Monoid (Par (TransitiveRelation a)) where
  mempty = parEmptyGraph

instance (Ord a) => Semigroup (Seq (TransitiveRelation a)) where
  (<>) = seqGraph

instance (Ord a) => Monoid (Seq (TransitiveRelation a)) where
  mempty = seqEmptyGraph

instance (Ord a) => Duoid.Normal (TransitiveRelation a)

-- reflexive relation

instance (Ord a) => Semigroup (Par (ReflexiveRelation a)) where
  (<>) = parGraph

instance (Ord a) => Monoid (Par (ReflexiveRelation a)) where
  mempty = parEmptyGraph

instance (Ord a) => Semigroup (Seq (ReflexiveRelation a)) where
  (<>) = seqGraph

instance (Ord a) => Monoid (Seq (ReflexiveRelation a)) where
  mempty = seqEmptyGraph

instance (Ord a) => Duoid.Normal (ReflexiveRelation a)

-- preorder relation

instance (Ord a) => Semigroup (Par (PreorderRelation a)) where
  (<>) = parGraph

instance (Ord a) => Monoid (Par (PreorderRelation a)) where
  mempty = parEmptyGraph

instance (Ord a) => Semigroup (Seq (PreorderRelation a)) where
  (<>) = seqGraph

instance (Ord a) => Monoid (Seq (PreorderRelation a)) where
  mempty = seqEmptyGraph

instance (Ord a) => Duoid.Normal (PreorderRelation a)

-- todo

instance (Ord a) => Semigroup (Par (Todo a)) where
  (<>) = parGraph

instance (Ord a) => Monoid (Par (Todo a)) where
  mempty = parEmptyGraph

instance (Ord a) => Semigroup (Seq (Todo a)) where
  (<>) = seqGraph

instance (Ord a) => Monoid (Seq (Todo a)) where
  mempty = seqEmptyGraph

instance (Ord a) => Duoid.Normal (Todo a)

-- labeled adjacency map

instance
  (Dioid e, Eq e, Ord a) =>
  Semigroup (Par (Labeled.AdjacencyMap e a))
  where
  (<>) = parGraph

instance (Dioid e, Eq e, Ord a) => Monoid (Par (Labeled.AdjacencyMap e a)) where
  mempty = parEmptyGraph

instance (Dioid e, Eq e, Ord a) => Semigroup (Seq (Labeled.AdjacencyMap e a)) where
  (<>) = seqGraph

instance (Dioid e, Eq e, Ord a) => Monoid (Seq (Labeled.AdjacencyMap e a)) where
  mempty = seqEmptyGraph

instance (Dioid e, Eq e, Ord a) => Duoid.Normal (Labeled.AdjacencyMap e a)

-- labeled graph

instance (Dioid e) => Semigroup (Par (Labeled.Graph e a)) where
  (<>) = parGraph

instance (Dioid e) => Monoid (Par (Labeled.Graph e a)) where
  mempty = parEmptyGraph

instance (Dioid e) => Semigroup (Seq (Labeled.Graph e a)) where
  (<>) = seqGraph

instance (Dioid e) => Monoid (Seq (Labeled.Graph e a)) where
  mempty = seqEmptyGraph

instance (Dioid e) => Duoid.Normal (Labeled.Graph e a)
