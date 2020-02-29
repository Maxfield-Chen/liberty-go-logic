{ mkDerivation, base, containers, gdp, HUnit, lens, miso, mtl, sort
, stdenv, transformers
}:
mkDerivation {
  pname = "bg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers gdp lens mtl transformers
  ];
  executableHaskellDepends = [ base miso ];
  testHaskellDepends = [
    base containers gdp HUnit lens mtl sort transformers
  ];
  homepage = "https://github.com/Maxfield-Chen/liberty-go-server";
  license = stdenv.lib.licenses.bsd3;
}
