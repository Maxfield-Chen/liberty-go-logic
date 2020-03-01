with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/1.4.tar.gz";
  sha256 = "1wl9vpqxshzrlanm9rpvgkiay3xa1abvkyknnp5z41kgfw63ypdl";
}) {});

let 
 overrides = self: super: {
        bg = 
          self.callPackage ./bg.nix { };
        gdp = 
          self.callPackage ./nix/gdp.nix { };
          };
 newPkgs = pkgs.haskellPackages.override { inherit overrides; };
in
  { bg = newPkgs.bg;
  }
