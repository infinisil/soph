{ nixpkgs ? builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/4b649a99d8461c980e7028a693387dc48033c1f7.tar.gz";
    sha256 = "0iy2gllj457052wkp20baigb2bnal9nhyai0z9hvjr3x25ngck4y";
  }
, pkgs ? import nixpkgs {}
, lib ? pkgs.lib
}:

with lib;

let

  hpkgs = import ./stack2nix.nix { inherit pkgs; };

  src = builtins.filterSource (absName: type: let
    name = removePrefix "${toString ./.}/" absName;
  in ! (hasSuffix ".stack-work" name
    || hasSuffix ".git" name
    || name == "result"
    || hasSuffix ".nix" name)
  ) ./.;

  hashsearch = hpkgs.hashsearch.overrideAttrs (oldAttrs: {
    inherit src;
  });

  wrapped = pkgs.symlinkJoin {
    name = "hashsearch";
    paths = [ hashsearch ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/hashsearch \
        --prefix PATH : "${makeBinPath [ pkgs.blockhash pkgs.feh ]}"
    '';
  };

in
  wrapped
