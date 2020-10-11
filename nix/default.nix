{ sources ? import ./sources.nix
}:

let
  pkgs = import sources.nixpkgs {};

  # gitignore.nix
  gitignoreSource = (import sources."gitignore.nix" { inherit (pkgs) lib; }).gitignoreSource;
  src = gitignoreSource ./..;
  haskell = pkgs.haskell.packages.ghc865;
in
  with pkgs; {
    inherit pkgs haskell src;


    pixie = haskell.callPackage ./pixie.nix {};

    # provided by shell.nix
    devTools = [
      bashInteractive
      cabal2nix
      ghcid
      haskell.cabal-install
      haskell.fast-tags
      hlint
      niv
      ormolu
      pre-commit
    ];

    # to be built by github actions
    ci = {
      pre-commit-check = (import sources."pre-commit-hooks.nix").run {
        inherit src;

        hooks = {
          shellcheck.enable = true;
          nixpkgs-fmt.enable = true;
          nix-linter.enable = true;
          ormolu.enable = true;
          hlint.enable = true;
          cabal-fmt.enable = true;
        };
        # generated files
        excludes = [ "^nix/sources\.nix$" ];
      };

    };
  }
