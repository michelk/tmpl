# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, template, text }:

cabal.mkDerivation (self: {
  pname = "tmpl";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ template text ];
  meta = {
    homepage = "https://www.github.com/michelk/tmpl";
    description = "simple executable for templating";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
