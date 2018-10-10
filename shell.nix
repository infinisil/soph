with import ./. {};

shellFor {
  packages = p: [ hashsearch ];
  nativeBuildInputs = [ cabal-install ];
}
