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

    {ignore = {name = "Use list comprehension";};}
    ## This is because duoids give `pure` and `return` distinct semantics.
    {ignore = {name = "Use pure";};}

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
            hint = {
              lhs = "maybe (return ())";
              rhs = "mapM_";
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
