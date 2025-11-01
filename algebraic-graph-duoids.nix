{ mkDerivation, algebraic-graphs, base, Cabal, cabal-doctest
, doctest, duoids, duoids-hedgehog, hedgehog, lib, no-recursion
}:
mkDerivation {
  pname = "algebraic-graph-duoids";
  version = "0.0.1.0";
  src = ./algebraic-graph;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    algebraic-graphs base duoids no-recursion
  ];
  testHaskellDepends = [
    algebraic-graphs base doctest duoids-hedgehog hedgehog no-recursion
  ];
  doBenchmark = true;
  homepage = "https://github.com/sellout/duoids#readme";
  description = "Duoid instances for the algebraic-graphs package";
  license = "(AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial)";
  maintainers = [ lib.maintainers.sellout ];
}
