{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    reflex-vty = {
      url = "github:ners/reflex-vty/patch-1";
      flake = false;
    };
    lsp = {
      url = "github:ners/lsp";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      with builtins;
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        lib = inputs.nixpkgs.lib;
        pname = "dosh";
        src = inputs.nix-filter.lib {
          root = ./.;
          include = [
            "app"
            "src"
            "test"
            "${pname}.cabal"
            "CHANGELOG.md"
            "LICENCE"
          ];
        };
        haskellPackagesOverride = ps: ps.override {
          overrides = self: super:
            with pkgs.haskell.lib;
            let ghcVersionAtLeast = lib.versionAtLeast ps.ghc.version; in
            builtins.trace "GHC version: ${ps.ghc.version}"
              ({
                dosh = self.callCabal2nix pname src { };
                reflex-process = doJailbreak super.reflex-process;
                reflex-vty = self.callCabal2nix "reflex-vty" inputs.reflex-vty { };
                lsp-test = appendPatches super.lsp-test [
                  (pkgs.fetchpatch {
                    url = "https://github.com/ners/lsp/commit/ed0b77db3579149110ca1f3e9221fa1e89228a00.patch";
                    hash = "sha256-K62i/8IqbGzg4iUGONy9u9mVhYeRqOy7tsnCHQqsGus=";
                    stripLen = 1;
                  })
                ];
              } // lib.optionalAttrs (ghcVersionAtLeast "9.4") {
                ghc-syntax-highlighter = super.ghc-syntax-highlighter_0_0_9_0;
                mmorph = doJailbreak super.mmorph;
                reflex = doJailbreak super.reflex_0_9_0_0;
                string-qq = doJailbreak super.string-qq;
              });
        };
        outputsFor =
          { haskellPackages
          , name
          , pname ? ""
          , ...
          }:
          let ps = haskellPackagesOverride haskellPackages; in
          {
            packages.${name} = ps.${pname} or ps;
            devShells.${name} = ps.shellFor {
              packages = ps: [ ps.dosh ];
              withHoogle = true;
              nativeBuildInputs = with pkgs; with ps; [
                cabal-install
                fourmolu
                haskell-language-server
                nixpkgs-fmt
              ];
            };
            formatter = pkgs.nixpkgs-fmt;
          };
      in
      with lib;
      foldl' (acc: conf: recursiveUpdate acc (outputsFor conf)) { }
        (mapAttrsToList (name: haskellPackages: { inherit name haskellPackages; }) pkgs.haskell.packages ++ [
          {
            inherit (pkgs) haskellPackages;
            name = "defaultGhc";
          }
          {
            inherit pname;
            inherit (pkgs) haskellPackages;
            name = "default";
          }
        ])
    );
}
