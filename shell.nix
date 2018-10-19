with import ./. {};

hpkgs.shellFor {
  packages = p: [ hpkgs.hashsearch ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
}
