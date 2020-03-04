let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          gdp =
            haskellPackagesNew.callPackage ./nix/gdp.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
  haskellPackages = pkgs.haskellPackages;
  drv = haskellPackages.callCabal2nix "lgl" ./. { };

in
  { lgl = drv;
    lgl-shell = haskellPackages.shellFor {
      packages = p: [drv];
      buildInputs = with pkgs; [cabal-install hlint ghcid stylish-haskell];
      withHoogle = true;
      };
  }
