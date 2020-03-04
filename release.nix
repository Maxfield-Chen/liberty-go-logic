let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          lgl =
            haskellPackagesNew.callPackage ./lgl.nix { };

          gdp =
            haskellPackagesNew.callPackage ./nix/gdp.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { lgl = pkgs.haskellPackages.lgl;
  }
