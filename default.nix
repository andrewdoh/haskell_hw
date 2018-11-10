with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "ghc";
  builder = "${bash}/bin/bash";
  buildInputs = [ghc];
}
	