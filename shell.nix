with import ./. {};

shellFor {
  packages = p: with p; [ hashsearch ];
  withHoogle = true;
  nativeBuildInputs = [ cabal-install ];
}
