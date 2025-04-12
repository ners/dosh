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
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ]) root;
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
      src = sourceFilter ./.;
      pnames = cabalProjectPnames src;
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp:
          let
            version = getVersion hp.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
          in
          if hp ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.6" && versionOlder version "9.10"
          then acc // { ${ghcName} = hp; }
          else acc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      overlay = lib.composeManyExtensions [
        inputs.lsp-client.overlays.default
        inputs.terminal-widgets.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (cabalProjectOverlay src)
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
        let
          pkgs = pkgs'.extend overlay;
          hps = hpsFor pkgs;
          paths = lib.mapCartesianProduct
            ({ hp, pname }: hp.${pname})
            { hp = attrValues hps; pname = pnames; };
          pathsFor = hp: map (pname: hp.${pname}) pnames;
          pname = "dosh";
          bins = pkgs.buildEnv {
            name = "${pname}-bins";
            paths = pathsFor hps.default;
            pathsToLink = [ "/bin" ];
          };
          libs = pkgs.buildEnv {
            name = "${pname}-libs";
            inherit paths;
            pathsToLink = [ "/lib" ];
          };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.symlinkJoin {
            name = "${pname}-all";
            paths = [ bins libs ];
            inherit (hps.default.${pname}) meta;
          };
          devShells.${system} =
            foreach (hpsFor pkgs) (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = pathsFor;
                nativeBuildInputs = [
                  pkgs'.haskellPackages.cabal-install
                  hp.fourmolu
                  hp.haskell-language-server
                ];
              };
            });
        }
      );
}
