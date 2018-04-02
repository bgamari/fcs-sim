{ nixpkgs ? (import <nixpkgs> {}) }:

nixpkgs.haskellPackages.callCabal2nix "fcs-sim" ./. { }
