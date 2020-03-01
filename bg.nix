{ mkDerivation, aeson, base, containers, gdp, http-types, HUnit
, lens, lucid, miso, mtl, network-uri, servant, servant-lucid
, servant-server, sort, stdenv, text, transformers, wai
, wai-app-static, wai-extra, warp
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
  executableHaskellDepends = [
    aeson base http-types lucid miso mtl network-uri servant
    servant-lucid servant-server text wai wai-app-static wai-extra warp
  ];
  testHaskellDepends = [
    base containers gdp HUnit lens mtl sort transformers
  ];
  homepage = "https://github.com/Maxfield-Chen/liberty-go-server";
  license = stdenv.lib.licenses.bsd3;
}
