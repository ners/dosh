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
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      with builtins;
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        lib = inputs.nixpkgs.lib;
        src = inputs.nix-filter.lib {
          root = ./.;
          include = [
            "app"
            "src"
            "test"
            "dosh.cabal"
            "CHANGELOG.md"
            "LICENCE"
          ];
        };
        removeDots = version: concatStringsSep "" (splitVersion version);
        haskellPackagesOverride = ps: ps.override {
          overrides = self: super:
            with pkgs.haskell.lib;
            let ghcVersionAtLeast = lib.versionAtLeast ps.ghc.version; in
            builtins.trace "GHC version: ${ps.ghc.version}"
              ({
                dosh = self.callCabal2nix "dosh" src { };
                reflex-process = doJailbreak super.reflex-process;
                reflex-vty = self.callCabal2nix "reflex-vty" inputs.reflex-vty { };
              } // lib.optionalAttrs (ghcVersionAtLeast "9.4") {
                ghc-syntax-highlighter = super.ghc-syntax-highlighter_0_0_9_0;
                mmorph = doJailbreak super.mmorph;
                reflex = doJailbreak super.reflex_0_9_0_0;
                string-qq = doJailbreak super.string-qq;
              });
        };
        outputsFor =
          { haskellPackages
          , name ? "ghc" + removeDots haskellPackages.ghc.version
          , package ? ""
          , ...
          }:
          let ps = haskellPackagesOverride haskellPackages; in
          {
            packages.${name} = ps.${package} or ps;
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
          };
      in
      foldl' (acc: conf: lib.recursiveUpdate acc (outputsFor conf))
        {
          formatter = pkgs.nixpkgs-fmt;
        }
        ([
          {
            haskellPackages = pkgs.haskellPackages;
            name = "default";
            package = "dosh";
          }
        ] ++ lib.pipe pkgs.haskell.packages
          [
            attrValues
            (filter (ps: ps ? ghc))
            (map (ps: { haskellPackages = ps; }))
          ])
    );
}
