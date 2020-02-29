let 
  config = {
    packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override { 
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        bg = 
          haskellPackagesNew.callPackage ./bg.nix { };
        gdp = 
          haskellPackagesNew.callPackage ./gdp.nix { };
          };
        };
      };
    };
  pkgs = import <nixpkgs> { inherit config; };

in
  { bg = pkgs.haskellPackages.bg;
  }
