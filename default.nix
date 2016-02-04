{ mkDerivation, aeson, base, bytestring, containers, either
, http-types, lens, network-uri, QuickCheck, quickcheck-instances
, scientific, servant, servant-client, servant-mock, servant-server
, split, stdenv, text, transformers, unordered-containers, vector
, wai, warp
}:
mkDerivation {
  pname = "haskell-kubernetes";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers either http-types lens network-uri
    QuickCheck quickcheck-instances scientific servant servant-client
    servant-server split text unordered-containers vector wai
  ];
  executableHaskellDepends = [
    base either network-uri QuickCheck servant servant-client
    servant-mock servant-server split transformers warp
  ];
  homepage = "https://github.com/soundcloud/haskell-kubernetes";
  description = "Haskell bindings to the Kubernetes API (via swagger & servant)";
  license = stdenv.lib.licenses.mit;
}
