{ pkgs ? import <nixpkgs> {} }: pkgs.mkShell {
    nativeBuildInputs = with pkgs.buildPackages; [
        cabal-install
        ghc
        pkg-config
        zlib
        ghciwatch
    ];
    shellHook = /* bash */ ''
        alias  watch='ghciwatch \
            --command "cabal v2-repl cardfinity-designer" \
            --watch "cardfinity-designer" \
            --after-startup-ghci "main" \
            --after-reload-ghci "main" \
            --reload-glob "cardfinity-core/**/*" \
            --reload-glob "cardfinity-designer/*.cabal"
        '
    '';
    # ghciwatch --watch 'cardfinity-designer' --after-startup-ghci 'main' --after-reload-ghci 'main' --command 'cabal v2-repl cardfinity-designer'
}
