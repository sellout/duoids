{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Algebra.Graph.Duoid
  ( parEmptyGraph,
    seqEmptyGraph,
    parGraph,
    seqGraph,
  )
where

import "algebraic-graphs" Algebra.Graph.Class (Graph)
import "algebraic-graphs" Algebra.Graph.Class qualified as Graph
  ( connect,
    empty,
    overlay,
  )
import safe "base" Data.Function (($))
import safe "duoids" Data.Duoid (Par (Par), Seq (Seq))

parEmptyGraph :: (Graph g) => Par g
parEmptyGraph = Par Graph.empty

seqEmptyGraph :: (Graph g) => Seq g
seqEmptyGraph = Seq Graph.empty

parGraph :: (Graph g) => Par g -> Par g -> Par g
parGraph (Par x) (Par y) = Par $ Graph.overlay x y

seqGraph :: (Graph g) => Seq g -> Seq g -> Seq g
seqGraph (Seq x) (Seq y) = Seq $ Graph.connect x y
