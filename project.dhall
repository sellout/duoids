{-| Project-wide defaults for Cabal.

This includes things like dependency ranges, licensing, etc. It should be merged
with the package.dhall file in each subdirectory.

FIXME:
  - needed to add no-recursion to places it got clobbered (need to use `combine` operations)
  - no `laws` suite for transformer is written yet
  - would be good to remove default extensions from `readme` (force us to specify the required ones in the examples)
-}
let D = ./dependencies.dhall

let Pvp = D.PVP

let ghc = D.GHC.Release

let LanguageEdition = D.GHC.LanguageEdition

let hpack = D.hpack

let v = Pvp.v

let Category = D.Hackage.Category.Type

let -- TODO: Remove this once sol/hpack#624 is released.
    showCategories =
      λ(categories : List Category) →
        if    Natural/isZero (List/length Category categories)
        then  None Text
        else  Some
                ( D.Prelude.Text.concatMapSep
                    ", "
                    Category
                    D.Hackage.Category.show
                    categories
                )

let global = D.hpack-dhall-defaults

let minimumGhcVersion = ghc.v9-6-1

let rec = λ(_ : Type → Type) → <>

let mid = λ(f : Type → Type) → f (rec f)

let extra = λ(f : Type → Type) → f (f (rec f))

let globalDefaults = global.schemas rec minimumGhcVersion.version

let allGhcVersions =
      D.Prelude.List.map
        ghc.Type
        Pvp.Version
        (λ(v : ghc.Type) → v.version)
        [ minimumGhcVersion
        , ghc.v9-6-3
        , ghc.v9-8-1
        , ghc.v9-8-4
        , ghc.v9-10-1
        , ghc.v9-12-1
        ]

let deps =
      { algebraic-graph-duoids = [] : List Pvp.Version
      , algebraic-graphs = [ v [ 0, 6, 1 ], v [ 0, 7 ] ]
      , base =
        [ v [ 4, 18, 0 ], v [ 4, 19, 0 ], v [ 4, 20, 0 ], v [ 4, 21, 0 ] ]
      , cabal-doctest = [ v [ 1, 0, 0 ] ]
      , doctest = [ v [ 0, 21, 1 ], v [ 0, 22, 6 ], v [ 0, 24, 0 ] ]
      , duoids = [] : List Pvp.Version
      , duoids-hedgehog = [] : List Pvp.Version
      , hedgehog = [ v [ 1, 2 ], v [ 1, 4 ], v [ 1, 5 ], v [ 1, 7 ] ]
      , hspec-expectations = [] : List Pvp.Version
      , markdown-unlit = [] : List Pvp.Version
      , no-recursion = [ v [ 0, 2, 0 ] ]
      , transformer-duoids = [] : List Pvp.Version
      , transformers = [ v [ 0, 6, 1 ] ]
      }

let defaultCombiners =
      { buildable = hpack.Combine.discardConflict Bool
      , synopsis = hpack.Combine.discardConflict Text
      , description = hpack.Combine.discardConflict Text
      , category = hpack.Combine.discardConflict Text
      , stability = hpack.Combine.discardConflict Text
      , homepage = hpack.Combine.discardConflict Text
      , bug-reports = hpack.Combine.discardConflict Text
      , license = hpack.Combine.discardConflict hpack.License.Type
      , build-type = hpack.Combine.discardConflict hpack.BuildType.Type
      , data-dir = hpack.Combine.discardConflict Text
      , github = hpack.Combine.discardConflict Text
      , git = hpack.Combine.discardConflict Text
      , custom-setup = hpack.Combine.discardConflict hpack.CustomSetup.Type
      , library = hpack.Combine.discardConflict (extra hpack.Library.Type)
      , executable = hpack.Combine.discardConflict (extra hpack.Executable.Type)
      }

let hpackSchemas = hpack.schemas mid

let combine =
      { Library =
          hpack.Library.combine
            (mid hpack.Library.Type)
            (   defaultCombiners.{ buildable }
              ∧ { exposed = λ(x : Bool) → λ(y : Bool) → x && y }
            )
      , Package = hpack.Package.combine extra defaultCombiners
      , Sublibrary =
          hpack.Sublibrary.combine
            (mid hpack.Sublibrary.Type)
            (   defaultCombiners.{ buildable }
              ∧ { exposed = λ(x : Bool) → λ(y : Bool) → x && y
                , visibility =
                    λ(_ : hpack.Visibility.Type) →
                    λ(y : hpack.Visibility.Type) →
                      y
                }
            )
      , Test =
          hpack.Test.combine
            (mid hpack.Test.Type)
            (   defaultCombiners.{ buildable }
              ∧ { main = hpack.Combine.discardConflict Text }
            )
      }

let Test =
      combine.Test
        ( combine.Test
            globalDefaults.Binary.default
            hpackSchemas.Binary::( global.Common.disableRecursion
                                     (mid hpack.Test.Type)
                                     deps.no-recursion
                                     ([] : List < Disable >)
                                 )
        )

let DefaultPackage =
      { Type = hpack.Package.Default.Type extra
      , default = hpack.Package.Default.default extra
      }

let Package =
      λ(name : Text) →
      λ(version : Text) →
      λ(synopsis : Text) →
      λ(description : Text) →
      λ(extraCategories : List Category) →
      λ(library : hpack.Library.Type (hpack.Library.Type <>)) →
      λ(extraPackage : hpack.Package.Default.Type extra) →
      λ(extraDoctestsDeps : D.Prelude.Map.Type Text (List Pvp.Version)) →
        let categories =
              [ Category.Algebra, Category.`Error Handling` ] # extraCategories

        in    combine.Package
                ( combine.Package
                    globalDefaults.Package.default
                    DefaultPackage::{
                    , synopsis = Some synopsis
                    , description = Some description
                    , library = Some library
                    , copyright = [ "2024 Greg Pfeil" ]
                    , homepage = Some "https://github.com/sellout/duoids#readme"
                    , bug-reports = Some
                        "https://github.com/sellout/duoids/issues"
                    , category = showCategories categories
                    , github = Some
                        ( hpack.Github.show
                            hpack.Github::{ owner = "sellout", repo = "duoids" }
                        )
                    , tested-with =
                        D.Prelude.List.map
                          Pvp.Version
                          Text
                          (λ(v : Pvp.Version) → "GHC == " ++ Pvp.show v)
                          allGhcVersions
                    , custom-setup = Some
                      { dependencies =
                            [ hpack.Dependency::{
                              , name = "Cabal"
                              , -- This needs to match the `cabal-version` field in
                                -- the generated Cabal file. See haskell/cabal#3751.
                                version = Some
                                  ">= 3.0.0"
                              }
                            ]
                          # D.hall.Cabal.buildPvpDependencies
                              (toMap deps.{ base, cabal-doctest })
                      }
                    , verbatim =
                      [ hpack.Verbatim.Structure.Object
                          ( toMap
                              { -- NB: Needed to support the Universal FOSS
                                --     exception license addition.
                                cabal-version = hpack.Verbatim.Type.String "3.0"
                              }
                          )
                      ]
                    , tests = toMap
                        { -- TODO: `globalDefaults.doctests` should be a more
                          --       basic doctest, so we can use `Test
                          --       (globalDefaults.doctests …)` here without
                          --       creating duplication.
                          doctests =
                            combine.Test
                              ( globalDefaults.doctests
                                  deps.{ base, doctest }
                                  { dependencies =
                                        [ { mapKey = name
                                          , mapValue = [] : List Pvp.Version
                                          }
                                        ]
                                      # extraDoctestsDeps
                                  , when =
                                      [] : List
                                             ( hpack.Conditional.Type
                                                 (hpack.Test.Type <>)
                                             )
                                  }
                              )
                              hpackSchemas.Binary::( global.Common.disableRecursion
                                                       (mid hpack.Test.Type)
                                                       deps.no-recursion
                                                       ([] : List < Disable >)
                                                   )
                        }
                    }
                )
                extraPackage
            ∧ { name, version }

in  { DefaultPackage
    , Package
    , Library =
        combine.Library
          ( combine.Library
              ( combine.Library
                  globalDefaults.Library.default
                  hpackSchemas.Library::( global.Common.disableRecursion
                                            (mid hpack.Library.Type)
                                            deps.no-recursion
                                            ([] : List < Disable >)
                                        )
              )
              hpackSchemas.Library::{ source-dirs = [ "src" ] }
          )
    , Sublibrary = combine.Sublibrary globalDefaults.Sublibrary.default
    , Test
    , -- this is exposed as a variable in the template, lets us define common
      -- ranges and mixins for dependencies across the project
      deps
    , -- Not all packages have laws tests, so this just sets up a template that
      -- each package can choose to use.
      laws =
        λ(extraDeps : D.Prelude.Map.Type Text (List Pvp.Version)) →
          Test
            hpackSchemas.Binary::{
            , main = Some "tests/laws.hs"
            , dependencies =
                D.hall.Cabal.buildPvpDependencies
                  (toMap deps.{ base, duoids-hedgehog, hedgehog } # extraDeps)
            }
    , -- TODO: `globalDefaults.readme`  should be a more basic readme, so we can
      --       use `Test (globalDefaults.readme …)` here without creating
      --       duplication.
      readme =
        λ(dependencies : D.Prelude.Map.Type Text (List Pvp.Version)) →
          combine.Test
            ( globalDefaults.readme
                deps.{ markdown-unlit }
                { dependencies
                , when = [] : List (hpack.Conditional.Type (hpack.Test.Type <>))
                }
            )
            hpackSchemas.Binary::( global.Common.disableRecursion
                                     (mid hpack.Test.Type)
                                     deps.no-recursion
                                     ([] : List < Disable >)
                                 )
    , -- We pass around dependency sets a lot. This gives us a shorthand for the
      -- particularly noisy case where we don’t need to pass any.
      noDeps = D.Prelude.Map.empty Text (List Pvp.Version)
    , globalDefaults
    , hpack = hpack.schemas mid
    }
