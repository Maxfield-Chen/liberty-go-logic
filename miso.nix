{ mkDerivation, aeson, base, bytestring, containers, fetchgit
, http-api-data, http-types, lucid, network-uri, servant
, servant-lucid, stdenv, text, transformers, vector
}:
mkDerivation {
  pname = "miso";
  version = "1.4.0.0";
  src = fetchgit {
    url = "https://github.com/dmjio/miso/";
    sha256 = "1wl9vpqxshzrlanm9rpvgkiay3xa1abvkyknnp5z41kgfw63ypdl";
    rev = "f11b6a9eb8b69d71ac777975b13d3632d931f61e";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers http-api-data http-types lucid
    network-uri servant servant-lucid text transformers vector
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
