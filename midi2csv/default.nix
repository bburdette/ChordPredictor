# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, midi }:

cabal.mkDerivation (self: {
  pname = "midi2csv";
  version = "0.1.0.0";
  src=./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ midi ];
  meta = {
    description = "midi in, csv out";
    license = self.stdenv.lib.licenses.gpl2;
    platforms = self.ghc.meta.platforms;
  };
})
