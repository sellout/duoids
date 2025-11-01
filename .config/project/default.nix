{
  config,
  flaky,
  lib,
  pkgs,
  self,
  supportedSystems,
  ...
}: {
  project = {
    name = "duoids";
    summary = "Unifying parallel and sequential operations";
    devPackages = [pkgs.dhall];

    ## Copy the license files to each package.
    ## TODO: Make this much more automatic.
    file = {
      "core/LICENSE".source = ../../LICENSE;
      "core/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
      "core/LICENSE.Universal-FOSS-exception-1.0".source = ../../LICENSE.Universal-FOSS-exception-1.0;
      "core/LICENSE.commercial".source = ../../LICENSE.commercial;
      "core/README.lhs".source = ../../core/README.md;
      "algebraic-graph/LICENSE".source = ../../LICENSE;
      "algebraic-graph/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
      "algebraic-graph/LICENSE.Universal-FOSS-exception-1.0".source = ../../LICENSE.Universal-FOSS-exception-1.0;
      "algebraic-graph/LICENSE.commercial".source = ../../LICENSE.commercial;
      "algebraic-graph/README.lhs".source = ../../algebraic-graph/README.md;
      "hedgehog/LICENSE".source = ../../LICENSE;
      "hedgehog/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
      "hedgehog/LICENSE.Universal-FOSS-exception-1.0".source = ../../LICENSE.Universal-FOSS-exception-1.0;
      "hedgehog/LICENSE.commercial".source = ../../LICENSE.commercial;
      "hedgehog/README.lhs".source = ../../hedgehog/README.md;
      "transformer/LICENSE".source = ../../LICENSE;
      "transformer/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
      "transformer/LICENSE.Universal-FOSS-exception-1.0".source = ../../LICENSE.Universal-FOSS-exception-1.0;
      "transformer/LICENSE.commercial".source = ../../LICENSE.commercial;
      "transformer/README.lhs".source = ../../transformer/README.md;
    };
  };

  imports = [./hlint.nix];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git.enable = true;
  };

  ## formatting
  editorconfig.enable = true;

  programs = {
    treefmt = {
      enable = true;
      ## TODO: See numtide/treefmt-nix#419, but with any luck, prettier works on
      ##       i686-linux again …
      programs.prettier.enable = lib.mkForce true;
    };
    vale.enable = true;
  };

  ## building
  programs.hpack-dhall = {
    enable = true;
    dhallPackage = {
      core = {};
      algebraic-graph = {};
      hedgehog = {};
      transformer = {};
    };
  };
  programs.cabal2nix = {
    enable = true;
    cabalPackage = {
      ## TODO: The default location for the .nix files generated here should be
      ##       like .local/state/project-manager/cabal2nix/<package-name>.nix.
      ## TODO: And we should also generate
      ##       .local/state/project-manager/cabal2nix/default.nix that loads all
      ##       of these derivations. Ideally, that would be able to distinguish
      ##       local vs remote (maybe just an explicit field passed here, or
      ##       maybe identified from the source) so we can pass in overrides,
      ##       etc. that apply only to local packages.
      duoids = {
        benchmark = true;
        maintainers = ["sellout"];
        source = "core";
      };
      algebraic-graph-duoids = {
        benchmark = true;
        maintainers = ["sellout"];
        source = "algebraic-graph";
      };
      duoids-hedgehog = {
        benchmark = true;
        maintainers = ["sellout"];
        source = "hedgehog";
      };
      transformer-duoids = {
        benchmark = true;
        maintainers = ["sellout"];
        source = "transformer";
      };
      ## TODO: We need to support cabal2nix for external packages, too, because
      ##       things like `callHackageDirect` also use IFD. But right now the
      ##       PM branch doesn’t handle these URLs correctly. (But I think it’s
      ##       the same reason that the nix files end up with absolute paths, so
      ##       we should be able to fix those together.)
      # hpack-dhall.source = "cabal://hpack-dhall-0.5.7";
      # no-recursion.source = "cabal://no-recursion-0.2.0.0";
    };
  };

  ## CI
  services.garnix.enable = true;
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    ([
        "All Garnix checks"
        "check-bounds"
        "check-licenses"
      ]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      config.services.haskell-ci.systems);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    ## Versions required by Nixpkgs 25.05, but not selected by GitHub jobs.
    extraDependencyVersions = ["doctest-0.22.6" "doctest-0.24.0"];
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {
      algebraic-graph-duoids = "algebraic-graph";
      # duoidal-categories = "categories";
      duoids = "core";
      duoids-hedgehog = "hedgehog";
      transformer-duoids = "transformer";
    };
    latestGhcVersion = "9.10.1";
  };
  ## TODO: Temporarily disabled Nix CI due to a job proliferation issue.
  services.nix-ci.enable = lib.mkForce false;

  ## publishing
  services.github.enable = true;
  services.github.settings.repository.topics = [
    "algebraic-structures"
    "concurrency"
  ];
}
