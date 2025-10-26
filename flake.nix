{
  description = "dosh: the power of Haskell in your terminal!";

  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
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
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" "ftl" ])
          root;
      };
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp':
          let
            hp = tryEval hp';
            version = getVersion hp.value.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
          in
          if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.12"
          then acc // { ${ghcName} = hp.value; }
          else acc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      pnames = map (path: baseNameOf (dirOf path)) (lib.fileset.toList (lib.fileset.fileFilter (file: file.hasExt "cabal") ./.));
      haskell-overlay = lib.composeManyExtensions [
        inputs.lsp-client.overlays.haskell
        inputs.terminal-widgets.overlays.haskell
        (hfinal: hprev: lib.genAttrs pnames (pname: hfinal.callCabal2nix pname (sourceFilter ./${pname}) { }))
      ];
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              haskell-overlay
            ];
          };
        })
      ];
    in
    {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    }
    //
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = hpsFor pkgs;
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = hps.default.dosh;
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: map (pname: ps.${pname}) pnames;
                nativeBuildInputs = with pkgs'; with haskellPackages; [
                  cabal-install
                  cabal-gild
                  fourmolu
                ] ++ lib.optionals (lib.versionAtLeast (lib.getVersion hp.ghc) "9.4") [
                  hp.haskell-language-server
                ];
              };
            });
        }
      );
}
