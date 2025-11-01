{-| Package-specific Cabal configuration.

This is used by [hpack-dhall](https://github.com/cabalism/hpack-dhall) to
generate the Cabal file. If you use Nix, `nix develop project-manager switch`
will ensure the Cabal file is current.
-}
let D = ../dependencies.dhall

let project = ../project.dhall

let Category = ../categories.dhall

in  project.Package
      "duoids"
      "0.0.1.0"
      "Unifying parallel and sequential operations"
      ''
      Duoids relate a pair of monoids, where one can be seen as “parallel” and
      the other “sequential”.
      ''
      ([] : List Category)
      ( project.Library
          project.hpack.Library::{
          , dependencies =
              D.hall.Cabal.buildPvpDependencies (toMap project.deps.{ base })
          , exposed-modules =
            [ "Control.Duoidal"
            , "Control.Duoidal.Do"
            , "Control.Duoidal.Either"
            , "Control.Duoidal.Laws"
            , "Data.Duoid"
            , "Data.Duoid.Laws"
            ]
          }
      )
      project.DefaultPackage::{
      , tests = toMap
          { readme =
              project.readme
                (toMap project.deps.{ base, duoids, hspec-expectations })
          }
      }
      project.noDeps
