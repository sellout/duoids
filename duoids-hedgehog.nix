{ mkDerivation, base, Cabal, cabal-doctest, doctest, duoids
, hedgehog, lib, no-recursion
}:
mkDerivation {
  pname = "duoids-hedgehog";
  version = "0.0.1.0";
  src = ./hedgehog;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [ base duoids hedgehog no-recursion ];
  testHaskellDepends = [ base doctest hedgehog no-recursion ];
  doBenchmark = true;
  homepage = "https://github.com/sellout/duoids#readme";
  description = "Utilities for testing your `Duoid` and `Duoidal` instances with Hedgehog";
  license = "(AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial)";
  maintainers = [ lib.maintainers.sellout ];
}
