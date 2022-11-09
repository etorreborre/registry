{ self, pkgs, stdenv, lib, haskellPackages }:
with haskellPackages;
  mkDerivation rec {
    pname = "registry";
    description = "data structure for assembling components";
    version = "0.3.3.4";
    license = lib.licenses.mit;
    src = ../../.;
    sha256 = "1x5ilikd9xxdhkzvvm5mklxrzx8vbyzzji4rqnw8lsgrxpzwca9d";
    libraryHaskellDepends = [
      base
      containers
      exceptions
      hashable
      mmorph
      mtl
      protolude
      resourcet
      semigroupoids
      semigroups
      template-haskell
      text
      transformers-base
    ];
    testHaskellDepends = [
      async
      base
      bytestring
      containers
      directory
      exceptions
      generic-lens
      hashable
      hedgehog_1_2
      io-memoize
      mmorph
      MonadRandom
      mtl
      multimap
      protolude
      random
      resourcet
      semigroupoids
      semigroups tasty
      tasty-discover
      tasty-hedgehog_1_3_1_0
      tasty-th
      template-haskell
      text
      transformers-base
      universum
    ];
    testToolDepends = [ tasty-discover ];
  }
