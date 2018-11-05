let
  nixpkgs = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/800184df21cfe4924e1e901f387cda49a9a8269c.tar.gz";
    sha256 = "0xwlbs3sffsjgbc00gadmyi14q1b7ww0fgj0nxa77dip27jzywn1";
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
