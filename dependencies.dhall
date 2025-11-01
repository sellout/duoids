let hall =
      https://raw.githubusercontent.com/sellout/hall/refs/heads/typed-hpack/dhall/package.dhall
        sha256:868b3fb027530fa92ef376894d90742474e77035e27800ed98e9d9c62ad3b597

in  { Cabal = hall.Cabal
    , GHC = hall.GHC
    , Hackage = hall.Hackage
    , Prelude =
        https://prelude.dhall-lang.org/v20.1.0/package.dhall
          sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98
    , hall
    , PVP = hall.PVP
    , hpack = hall.hpack
    , hpack-dhall-defaults =
        https://raw.githubusercontent.com/sellout/hpack-dhall-defaults/refs/heads/typed-hpack/dhall/package.dhall
          sha256:cbae76f178f1d1b4a410266d3677e4559bd619f3b5938ffabae67508fe20a36a
    }
