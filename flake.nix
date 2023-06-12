{
  description = "dosh: the power of Haskell in your terminal!";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://dosh.cachix.org";
    extra-trusted-public-keys = "dosh.cachix.org-1:wRNFshU1IQW71/P0ueRqOdPqzsff/eGNl2MNKpsZy/o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      with builtins;
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        lib = inputs.nixpkgs.lib;
        src = pname: inputs.nix-filter.lib {
          root = "${./.}/${pname}";
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
                dosh-prelude = self.callCabal2nix "dosh-prelude" (src "dosh-prelude") { };
                dosh = self.callCabal2nix "dosh" (src "dosh") { };
                lsp-client = self.callCabal2nix "lsp-client" (src "lsp-client") { };
                reflex-process = doJailbreak super.reflex-process;
                reflex-vty = markUnbroken super.reflex-vty;
                haskell-language-server = lib.pipe super.haskell-language-server [
                  (drv: drv.override { hls-ormolu-plugin = null; })
                  (drv: disableCabalFlag drv "ormolu")
                ];
              } // lib.optionalAttrs (ghcVersionAtLeast "9.4") {
                ghc-syntax-highlighter = super.ghc-syntax-highlighter_0_0_9_0;
                mmorph = doJailbreak super.mmorph;
                reflex = doJailbreak super.reflex_0_9_0_0;
                string-qq = doJailbreak super.string-qq;
              } // lib.optionalAttrs (ghcVersionAtLeast "9.6") {
                commutative-semigroups = doJailbreak super.commutative-semigroups;
                ed25519 = doJailbreak super.ed25519;
                ghc-trace-events = doJailbreak super.ghc-trace-events;
                hie-compat = doJailbreak super.hie-compat;
                indexed-traversable = doJailbreak super.indexed-traversable;
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
              packages = ps: with ps; [ dosh lsp-client ];
              withHoogle = true;
              nativeBuildInputs = with ps; [
                cabal-fmt
                cabal-install
                fourmolu
                haskell-language-server
                pkgs.cachix
                pkgs.nixpkgs-fmt
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
            pname = "dosh";
            inherit (pkgs) haskellPackages;
            name = "default";
          }
        ])
    );
}
