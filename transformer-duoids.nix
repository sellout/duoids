{ mkDerivation, base, Cabal, cabal-doctest, doctest, duoids, lib
, no-recursion, transformers
}:
mkDerivation {
  pname = "transformer-duoids";
  version = "0.0.1.0";
  src = ./transformer;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [ base duoids no-recursion transformers ];
  testHaskellDepends = [ base doctest no-recursion ];
  doBenchmark = true;
  homepage = "https://github.com/sellout/duoids#readme";
  description = "Extending the transformers package with duoids";
  license = "(AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial)";
  maintainers = [ lib.maintainers.sellout ];
}
