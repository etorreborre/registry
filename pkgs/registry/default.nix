{ self, pkgs, stdenv, lib, haskell }:

# Use ghc 9.24 as the default ghc (9.0.2) does not provide the required language features of GHC2021
#                 vvvvvv
(haskell.packages.ghc924.override {
  overrides = self: super: {

    # we fetch fresh sources directly from hackage (https://hackage.haskell.org/)
    # and build them in the same context

    # provide a more recent version of `tasty-hedgehog` to satisfy downstream dependencies
    tasty-hedgehog = self.callHackageDirect {
      pkg = "tasty-hedgehog";
      ver = "1.4.0.0";
      sha256 = "sha256-vd3dknvXEAFcDUJtoeXGhoJMY8kX0069oV8R6jDXP4w=";
    } {};

    # provide a more recent version of `hedgehog` to satisfy downstream dependencies
    hedgehog = self.callHackageDirect {
      pkg = "hedgehog";
      ver = "1.2";
      sha256 = "sha256-PL5DMK6eexXhaaYaEy+lBwR5n2+r82AO/mhbiNMtU8k=";
    } {};

    # provide a more recent version of `hedgehog`
    # as the older one provided in nixpkgs-stable requires a conflicting version of tasty-hedgehog
    universum = self.callHackageDirect {
      pkg = "universum";
      ver = "1.8.1";
      sha256 = "sha256-LTDD4UiarKYOVrdLGVxwjGZE8t/oSVNeDYnmymWuUWk=";
    } {};
  };
})
.callCabal2nix "registry" ../.. {}
