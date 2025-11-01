{-| Package-specific Cabal configuration.

This is used by [hpack-dhall](https://github.com/cabalism/hpack-dhall) to
generate the Cabal file. If you use Nix, `nix develop project-manager switch`
will ensure the Cabal file is current.
-}
let D = ../dependencies.dhall

let project = ../project.dhall

let Category = ../categories.dhall

in  project.Package
      "duoids-hedgehog"
      "0.0.1.0"
      "Utilities for testing your `Duoid` and `Duoidal` instances with Hedgehog"
      ''
      Whenever you write an instance, you should have unit tests to reassure
      you that your instance follows the relevant laws. With this package,
      that becomes simple for any of the classes provided by the duoids
      package. See algebraic-graph-duoids and transformer-duoids for usage of
      this package, in addition to the tests on this package itself.
      ''
      [ Category.Testing ]
      ( project.Library
          project.hpack.Library::{
          , dependencies =
              D.hall.Cabal.buildPvpDependencies
                (toMap project.deps.{ base, duoids, hedgehog })
          , exposed-modules = [ "Test.Duoid", "Test.Duoidal" ]
          }
      )
      project.DefaultPackage::{
      , tests = toMap { laws = project.laws project.noDeps }
      }
      project.noDeps
