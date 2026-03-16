{
  lib,
  pkgs,
  ...
}: {
  ## Haskell linter
  programs.treefmt.programs.hlint.enable = true;
  ## TODO: Wrap this to find our generated hlint config in the store.
  project.devPackages = [pkgs.hlint];
  project.file.".hlint.yaml".text = lib.pm.generators.toYAML {} [
    {
      group = {
        name = "dollar";
        enabled = true;
      };
    }
    {
      group = {
        name = "future";
        enabled = true;
      };
    }
    {
      group = {
        name = "generalise";
        enabled = true;
      };
    }

    {
      ignore.name = [
        ## This can be removed once we no longer support ≤ base 4.18, which
        ## doesn’t yet export `Data.Functor.unzip`.
        "Avoid NonEmpty.unzip"
        ## This complains when we use a common import and then import from the
        ## same module under a CPP conditional. Since Ormolu handles combinig
        ## imports when possible anyway, this warning isn’t helpful.
        "Use fewer imports"
        ## I just don’t like them (even the monadic variant).
        "Use list comprehension"
        ## These are because duoids give `Applicative`- and `Monad`-constrained
        ## operations distinct semantics.
        "Use pure"
        "Use traverse"
        "Use traverse_"
      ];
    }

    {
      package = {
        name = "monad";
        modules = ["import Control.Monad"];
      };
    }

    {
      package = {
        name = "traversable";
        modules = [
          "import Data.Foldable"
          "import Data.Traversable"
        ];
      };
    }

    {
      group = {
        name = "generalize";
        imports = [
          "package monad"
          "package traversable"
        ];
        rules = [
          {
            warn = {
              lhs = "map";
              rhs = "fmap";
            };
          }
        ];
      };
    }

    {
      group = {
        name = "generalize";
        imports = ["package traversable"];
        rules = [
          {
            hint = {
              lhs = "maybe (pure ())";
              rhs = "traverse_";
              note = "IncreasesLaziness";
            };
          }
          {
            warn = {
              lhs = "mappend";
              rhs = "(<>)";
            };
          }
          {
            warn = {
              lhs = "(++)";
              rhs = "(<>)";
            };
          }
        ];
      };
    }
  ];
}
