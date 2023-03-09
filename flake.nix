{
  description = "dosh: the power of Haskell in your terminal!";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://dosh.cachix.org";
    extra-trusted-public-keys = "dosh.cachix.org-1:wRNFshU1IQW71/P0ueRqOdPqzsff/eGNl2MNKpsZy/o=";
  };

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
    conduit = {
      url = "github:ners/conduit";
      flake = false;
    };
    conduit-parse = {
      url = "github:ners/conduit-parse";
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
                lsp-client = self.callCabal2nix pname ./lsp-client { };
                reflex-process = doJailbreak super.reflex-process;
                reflex-vty = self.callCabal2nix "reflex-vty" inputs.reflex-vty { };
                haskell-language-server = lib.pipe super.haskell-language-server [
                  (drv: drv.override { hls-ormolu-plugin = null; })
                  (drv: disableCabalFlag drv "ormolu")
                ];
                resourcet = appendPatch super.resourcet
                  (pkgs.fetchpatch {
                    # export MonadCatch
                    url = "https://github.com/ners/conduit/commit/325c3f55494213430b4c5be72687584bcdef0642.patch";
                    hash = "sha256-hmQFIT6mxhLX+bwffj2+gCpuOkBhBJuVYwDtGTzyBmc=";
                    stripLen = 1;
                  });
                conduit = appendPatches super.conduit [
                  (pkgs.fetchpatch {
                    # add missing instance MonadCatch ConduitT
                    url = "https://github.com/ners/conduit/commit/3aa5b684f2e1d845efa238a88cc1350f356727ac.patch";
                    hash = "sha256-a9uizqXGf5Jas3IAvcpDAMKE6aiBOh0nxRKqjoJN3Jw=";
                    stripLen = 1;
                  })
                  (pkgs.fetchpatch {
                    # expose ConduitT and Pipe constructors
                    url = "https://github.com/ners/conduit/commit/82de45dd7afe6b764a1f245498397c6baceffc09.patch";
                    hash = "sha256-UXz9/jDwqHGa9Lvgu85PQz2tV0EwriC4PPF8cVjpTXk=";
                    stripLen = 1;
                  })
                ];
                conduit-parse = self.callCabal2nix "conduit-parse" inputs.conduit-parse {};
                lsp-test = appendPatches super.lsp-test [
                  (pkgs.fetchpatch {
                    # add missing instance MonadCatch Session
                    url = "https://github.com/ners/lsp/commit/b082591a1bf0185104eb83bc1df55d86bd5f0225.patch";
                    hash = "sha256-+DvdmQLTQ63BWmyncBJCBHXD2FsDmE6PoSGDQ1CiGnQ=";
                    stripLen = 1;
                  })
                  (pkgs.fetchpatch {
                    # expose Session constructor
                    url = "https://github.com/ners/lsp/commit/1e5d0a5578c0a3a01c333b7b0c8da98877e55086.patch";
                    hash = "sha256-K62i/8IqbGzg4iUGONy9u9mVhYeRqOy7tsnCHQqsGus=";
                    stripLen = 1;
                  })
                  (pkgs.fetchpatch {
                    # change withTimout to take us, not s
                    url = "https://github.com/ners/lsp/commit/f8e4df587a7445f291b856f191bd51d8d91d2623.patch";
                    hash = "sha256-4Xc9/TgvbiP1ZVOM0M93YeUQgiFOFXHfo4EoqINUcQw=";
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
              packages = ps: with ps; [ dosh lsp-client ];
              withHoogle = true;
              nativeBuildInputs = with ps; [
                cabal-fmt
                cabal-install
                fourmolu
                haskell-language-server
                pkgs.cachix
                pkgs.nixpkgs-fmt
                pkgs.texlive.combined.scheme-full
              ];
              shellHook = ''
                export PATH=${
                  pkgs.writeShellScriptBin "haskell-language-server-wrapper" ''
                    tee -a vim-hls-input.log | ${ps.haskell-language-server}/bin/haskell-language-server-wrapper "$@" | tee -a vim-hls-output.log
                  ''
                }/bin:$PATH
              '';
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
