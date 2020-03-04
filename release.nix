let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          lgl =
            haskellPackagesNew.callPackage ./lgl.nix { };
          ghc = haskellPackagesOld.ghc // {withPackages = haskellPackagesOld.ghc.withHoogle; };
          ghcWithPackages = haskellPackagesNew.ghc.withPackages;
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
