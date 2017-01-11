with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "amkt-ansible-env";

  buildInputs = [
    haskellPackages.patat
  ];
}

