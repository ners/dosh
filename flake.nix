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
    haskell-language-server = {
      url = "github:ners/haskell-language-server/patch-1";
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
                knob = appendPatch super.knob
                  (pkgs.fetchpatch {
                    url = "https://github.com/ners/knob/commit/08312b5dc5214d5a3d6ad1d2463ed9d2736bc266.patch";
                    hash = "sha256-uNpYfuLuvVPztFoUmPez0FCMy5uI52bn3akIS+QRP9A=";
                  });
                ghcide = appendPatch super.ghcide
                  (pkgs.fetchpatch {
                    url = "https://github.com/haskell/haskell-language-server/commit/d36ec06382eaa0dfda384da123b0e1328de6853b.patch";
                    hash = "sha256-+guCHFdE68J5pfzznWpAZmD3lYXBntzXNntIQjYgxt0=";
                    stripLen = 1;
                  });
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
