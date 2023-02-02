{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
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
            {
              dosh = self.callCabal2nix "dosh" src { };
              reflex-vty = doJailbreak (markUnbroken super.reflex-vty);
            };
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
              nativeBuildInputs = with ps; [
                cabal-install
                fourmolu
                haskell-language-server
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
