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
                dosh-prelude = self.callCabal2nix pname ./dosh-prelude { };
                dosh = self.callCabal2nix pname src { };
                reflex-process = doJailbreak super.reflex-process;
                reflex-vty = self.callCabal2nix "reflex-vty" inputs.reflex-vty { };
                resourcet = appendPatch super.resourcet
                  (pkgs.fetchpatch {
                    # export MonadCatch
                    url = "https://github.com/ners/conduit/commit/325c3f55494213430b4c5be72687584bcdef0642.patch";
                    hash = "sha256-hmQFIT6mxhLX+bwffj2+gCpuOkBhBJuVYwDtGTzyBmc=";
                    stripLen = 1;
                  });
                conduit = appendPatch super.conduit
                  (pkgs.fetchpatch {
                    # add missing instance MonadCatch ConduitT
                    url = "https://github.com/ners/conduit/commit/3aa5b684f2e1d845efa238a88cc1350f356727ac.patch";
                    hash = "sha256-a9uizqXGf5Jas3IAvcpDAMKE6aiBOh0nxRKqjoJN3Jw=";
                    stripLen = 1;
                  });
                conduit-parse = appendPatch super.conduit-parse
                  (pkgs.fetchpatch {
                    # add missing instance MonadCatch ConduitParser
                    url = "https://github.com/ners/conduit-parse/commit/f485e2eaf3caf5d74fffdf97942022541e5bbe1e.patch";
                    hash = "sha256-ECK+utDmpGFbRekkkzQ3x2RGSo7MVkUi3WL78yakDBY=";
                  });
                lsp-test = appendPatch super.lsp-test
                  (pkgs.fetchpatch {
                    # add missing instance MonadCatch Session
                    url = "https://github.com/ners/lsp/commit/b082591a1bf0185104eb83bc1df55d86bd5f0225.patch";
                    hash = "sha256-+DvdmQLTQ63BWmyncBJCBHXD2FsDmE6PoSGDQ1CiGnQ=";
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
                cabal-fmt
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
