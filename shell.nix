let pkgs = import <nixpkgs> {};
    haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        midi2csv = self.callPackage ./. {};
      };
    };
 in pkgs.lib.overrideDerivation haskellPackages.midi2csv (attrs: {
   buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })
