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
        src = version: pkgs.buildEnv {
          name = "dosh-source";
          paths = [
            (inputs.nix-filter.lib {
              root = ./.;
              include = [
                "app"
                "src"
                "test"
                "dosh.cabal"
                "CHANGELOG.md"
                "LICENCE"
              ];
            })
          ];
          postBuild = ''
            sed -i 's/^\(version: .*\)$/\1.${version}/' $out/dosh.cabal
          '';
        };
        removeDots = version: concatStringsSep "" (splitVersion version);
        haskellPackagesOverride = ps: ps.override {
          overrides = self: super:
            {
              dosh = self.callCabal2nix "dosh" (src super.ghc.version) { };
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
              nativeBuildInputs = with ps; [
                cabal-install
                fourmolu
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
