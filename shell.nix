with import ./. {};

hpkgs.shellFor {
  packages = p: [ hpkgs.soph ];
  nativeBuildInputs = [ hpkgs.cabal-install ];
  shellHook = ''
    export NIX_PATH=nixpkgs=${toString pkgs.path}
  '';
}
