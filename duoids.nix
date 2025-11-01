{ mkDerivation, base, Cabal, cabal-doctest, doctest
, hspec-expectations, lib, markdown-unlit, no-recursion
}:
mkDerivation {
  pname = "duoids";
  version = "0.0.1.0";
  src = ./core;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [ base no-recursion ];
  testHaskellDepends = [
    base doctest hspec-expectations no-recursion
  ];
  testToolDepends = [ markdown-unlit ];
  doBenchmark = true;
  homepage = "https://github.com/sellout/duoids#readme";
  description = "Unifying parallel and sequential operations";
  license = "(AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial)";
  maintainers = [ lib.maintainers.sellout ];
}
