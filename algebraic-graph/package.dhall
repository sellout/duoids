{-| Package-specific Cabal configuration.

This is used by [hpack-dhall](https://github.com/cabalism/hpack-dhall) to
generate the Cabal file. If you use Nix, `nix develop project-manager switch`
will ensure the Cabal file is current.
-}
let D = ../dependencies.dhall

let project = ../project.dhall

let Category = ../categories.dhall

in  project.Package
      "algebraic-graph-duoids"
      "0.0.1.0"
      "Duoid instances for the algebraic-graphs package"
      ''
      Algebraic graphs are an interesting example of duoids. This provides the
      relevant instances.
      ''
      [ Category.Graphs ]
      ( project.Library
          project.hpack.Library::{
          , dependencies =
              D.hall.Cabal.buildPvpDependencies
                (toMap project.deps.{ algebraic-graphs, base, duoids })
          , exposed-modules = [ "Algebra.Graph.Orphans.Duoid" ]
          }
      )
      project.DefaultPackage::{
      , tests = toMap
          { laws =
              project.laws
                ( toMap
                    project.deps.{ algebraic-graph-duoids, algebraic-graphs }
                )
          }
      }
      project.noDeps
