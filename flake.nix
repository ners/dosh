{
  description = "dosh: the power of Haskell in your terminal!";

  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    lsp-client = {
      url = "github:ners/lsp-client";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rhine = {
      url = "github:turion/rhine";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    terminal-widgets = {
      url = "github:ners/terminal-widgets";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      hsSrc = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ] || file.type == "directory") ./.;
      };
      readDirs = root: attrNames (lib.filterAttrs (_: type: type == "directory") (readDir root));
      readFiles = root: attrNames (lib.filterAttrs (_: type: type == "regular") (readDir root));
      basename = path: suffix: with lib; pipe path [
        (splitString "/")
        last
        (removeSuffix suffix)
      ];
      cabalProjectPackages = root: with lib; foreach (readDirs root) (dir:
        let
          path = "${root}/${dir}";
          files = readFiles path;
          cabalFiles = filter (strings.hasSuffix ".cabal") files;
          pnames = map (path: basename path ".cabal") cabalFiles;
          pname = if pnames == [ ] then null else head pnames;
        in
        optionalAttrs (pname != null) { ${pname} = path; }
      );
      cabalProjectPnames = root: lib.attrNames (cabalProjectPackages root);
      cabalProjectOverlay = root: hfinal: hprev: with lib;
        mapAttrs
          (pname: path: hfinal.callCabal2nix pname path { })
          (cabalProjectPackages root);
      cabalPackageOverlay = name: root: hfinal: hprev: {
        ${name} = hfinal.callCabal2nix name root { };
      };
      project = hsSrc ./.;
      pnames = cabalProjectPnames project;
      hpsFor = pkgs: with lib;
        { default = pkgs.haskellPackages; }
        // filterAttrs
          (name: hp: match "ghc[0-9]{2}" name != null && versionAtLeast hp.ghc.version "9.2")
          pkgs.haskell.packages;
      overlay = lib.composeManyExtensions [
        inputs.lsp-client.overlays.lspOverlay
        inputs.terminal-widgets.overlays.default
        inputs.rhine.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (cabalProjectOverlay project)
              (hfinal: hprev: with prev.haskell.lib.compose; {
                lsp-test = dontCheck (hfinal.callHackageDirect
                  {
                    pkg = "lsp-test";
                    ver = "0.17.1.0";
                    sha256 = "sha256-1Onf7oQUDkgjfTAFNEfV7z+6gDMfcFhEFUaeGYRB7JI=";
                  }
                  { });
                ghcide = hfinal.callHackageDirect
                  {
                    pkg = "ghcide";
                    ver = "2.9.0.0";
                    sha256 = "sha256-6z+uAg4Hb5w4I092+XFl6RYX6Sjlh3iczu3X/eNEEWw=";
                  }
                  { };
                haskell-language-server = lib.pipe {} [
                  (hfinal.callHackageDirect
                    {
                      pkg = "haskell-language-server";
                      ver = "2.9.0.0";
                      sha256 = "sha256-x3emp0FIVoH3BUAIb89PXaaSl3XS4np/AvLzfngAv1Q=";
                    }
                  )
                  (overrideCabal (old: {
                    enableSharedExecutables = true;
                    doCheck = false;
                  }))
                ];
                hls-plugin-api = hfinal.callHackageDirect
                  {
                    pkg = "hls-plugin-api";
                    ver = "2.9.0.0";
                    sha256 = "sha256-SQExFwyVkXeqjh1O0ggB9fgVY4GcUtSACxTzOxFWz9k=";
                  }
                  { };
                hls-graph = hfinal.callHackageDirect
                  {
                    pkg = "hls-graph";
                    ver = "2.9.0.0";
                    sha256 = "sha256-weEHMN20Zto9wbPZg2IgTf1kw34h6ctMql0E1C7YS+o=";
                  }
                  { };
                hls-test-utils = hfinal.callHackageDirect
                  {
                    pkg = "hls-test-utils";
                    ver = "2.9.0.0";
                    sha256 = "sha256-/SsWTs/I4nhbUiSyJ4eQjRSceWT+ik/K9mTe9+VjTI8=";
                  }
                  { };
                Diff = hprev.Diff_0_5;
                aeson = doJailbreak hprev.aeson_2_2_3_0;
                attoparsec-aeson = hprev.attoparsec-aeson_2_2_2_0;
                fourmolu = doJailbreak hprev.fourmolu;
              })
            ];
          };
          inherit (hpsFor final) dosh;
        })
      ];
    in
    {
      overlays.default = overlay;
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let pkgs = pkgs'.extend overlay; in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.haskellPackages.dosh;
          devShells.${system} =
            foreach (hpsFor pkgs) (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: map (pname: ps.${pname}) pnames;
                nativeBuildInputs = with pkgs'.haskellPackages; [
                  cabal-install
                  fourmolu
                  haskell-language-server
                ];
              };
            });
        }
      );
}
