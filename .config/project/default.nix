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
