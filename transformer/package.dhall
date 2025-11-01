{-| Package-specific Cabal configuration.

This is used by [hpack-dhall](https://github.com/cabalism/hpack-dhall) to
generate the Cabal file. If you use Nix, `nix develop project-manager switch`
will ensure the Cabal file is current.

TODO: re-add this test suite
    , tests = toMap
        { laws =
            project.laws
              (toMap project.deps.{ transformer-duoids, transformers })
        }
-}
let D = ../dependencies.dhall

let project = ../project.dhall

let Category = ../categories.dhall

in  project.Package
      "transformer-duoids"
      "0.0.1.0"
      "Extending the transformers package with duoids"
      ''
      Duoidal transformers are a lot like monad transformers, but
      appear to work parallelism magic.
      ''
      [ Category.Other "Transformers" ]
      ( project.Library
          project.hpack.Library::{
          , dependencies =
              D.hall.Cabal.buildPvpDependencies
                (toMap project.deps.{ base, duoids, transformers })
          , exposed-modules = [ "Control.Duoidal.Trans.Orphans" ]
          }
      )
      project.DefaultPackage::{=}
      project.noDeps
