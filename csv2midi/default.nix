  let
    pkgs = import <nixpkgs> {};
  in
  { stdenv ? pkgs.stdenv, python ? pkgs.python, pythonIRClib ? pkgs.pythonIRClib, alsaLib ? pkgs.alsaLib, setuptools ? pkgs.python27Packages.setuptools }:
  
  stdenv.mkDerivation {
    name = "python-nix";
    version = "0.1.0.0";
    src = ./.;
    buildInputs = [ python pythonIRClib alsaLib setuptools ];
  }
