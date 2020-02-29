# Figure out how to include miso's ghcjs set as a dependency with my overrides while using cachix
let 
  config = {
    packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override { 
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        bg = 
          haskellPackagesNew.callPackage ./bg.nix { };
        gdp = 
          haskellPackagesNew.callPackage ./gdp.nix { };
        miso = 
          haskellPackagesNew.callPackage ./miso.nix { };
          };
        };
      };
    };
  pkgs = import <nixpkgs> { inherit config; };

in
  { bg = pkgs.haskellPackages.bg;
  }
