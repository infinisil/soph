let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/0a7e258012b60cbe530a756f09a4f2516786d370.tar.gz";
    sha256 = "1qcnxkqkw7bffyc17mqifcwjfqwbvn0vs0xgxnjvh9w0ssl2s036";
  };
in
{ pkgs ? import nixpkgs {}
}:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = (pkgs.haskellPackages.extend (hlib.packageSourceOverrides {
    hashsearch = lib.cleanSourceWith {
      filter = name: type: baseNameOf (toString name) != "dist"
        && baseNameOf (toString name) != "test"
        && ! lib.hasSuffix ".nix" name;
      src = lib.cleanSource ./.;
    };
  })).extend (self: super: {
    hashsearch = super.hashsearch.overrideAttrs (drv: {
      nativeBuildInputs = drv.nativeBuildInputs or [] ++ [ pkgs.makeWrapper ];
      postInstall = drv.postInstall or "" + ''
        wrapProgram $out/bin/hashsearch \
          --prefix PATH : "${pkgs.lib.makeBinPath [ pkgs.feh ]}"
      '';
    });

    blockhash = hlib.doJailbreak super.blockhash;

    tasty-travis = hlib.doJailbreak super.tasty-travis;

    broadcast-chan = self.callHackage "broadcast-chan" "0.2.0.1" {};

    broadcast-chan-tests = hlib.addBuildDepend super.broadcast-chan-tests self.broadcast-chan;
  });
in
  hpkgs.hashsearch // {
    inherit pkgs hpkgs;
  }
