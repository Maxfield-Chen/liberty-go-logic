{ mkDerivation, base, containers, gdp, HUnit, lens, mtl, sort
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
  executableHaskellDepends = [ base gdp ];
  testHaskellDepends = [
    base containers gdp HUnit lens mtl sort transformers
  ];
  homepage = "https://github.com/githubuser/bg#readme";
  license = stdenv.lib.licenses.bsd3;
}
