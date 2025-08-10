{ pkgs ? import <nixpkgs> {} }: pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages; [
        cabal-install
        ghc
        pkg-config
        zlib
    ];
}
