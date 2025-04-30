{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  # Only the absolutely essential packages
  buildInputs = with pkgs; [
    ghc
    cabal-install
  ];

  shellHook = ''
    echo "==================================================="
    echo "  Haskell Chess Terminal Environment"
    echo "==================================================="
    echo "Build with:    cabal build"
    echo "Run:           cabal run"
    echo "==================================================="
  '';
}
