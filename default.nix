let
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/46651b82b87318e37440c15a639d49ec05e79b79.tar.gz";
    sha256 = "15wh0b6xn1bp94jfgw1b0s1j23x75c297d3ydz56fsvihlddxzd7";
  };

  nixpkgs = (import nixpkgsSrc {}).srcOnly {
    name = "nixpkgs-patched";
    src = nixpkgsSrc;
    patches = [
      # https://github.com/NixOS/nixpkgs/pull/46453
      (builtins.fetchurl {
        url = "https://github.com/NixOS/nixpkgs/commit/e6dd03d554e65badd9cdc8b9c137a5998a642e42.patch";
        sha256 = "0aisra3arv6x6z59dfw4bfxyj40pm6liixgkwpj1rjrr0ql4yc9s";
      })
    ];
  };
in
{ pkgs ? import nixpkgs {}
}:

(pkgs.haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
  hashsearch = ./.;
})).extend (self: super: {
  hashsearch = super.hashsearch.overrideAttrs (drv: {
    nativeBuildInputs = drv.nativeBuildInputs or [] ++ [ pkgs.makeWrapper ];
    postInstall = drv.postInstall or "" + ''
      wrapProgram $out/bin/hashsearch \
        --prefix PATH : "${pkgs.lib.makeBinPath [ pkgs.blockhash pkgs.feh ]}"
    '';
  });

  blockhash = pkgs.haskell.lib.doJailbreak super.blockhash;
}) // { inherit pkgs; }
