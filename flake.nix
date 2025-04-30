{
  description = "chess-hs: A chess game in Haskell with GTK interface";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Use Haskell's own methods for adding dependencies
        hlib = pkgs.haskell.lib;

        # Define GTK system dependencies
        gtkDeps = with pkgs; [
          gtk3
          glib
          cairo
          pango
        ];

        # Customizing the Haskell package set
        myHaskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            # Make chess-hs depend on our system dependencies
            chess-hs = hlib.compose.overrideCabal (drv: {
              librarySystemDepends = (drv.librarySystemDepends or [ ]) ++ gtkDeps;
              libraryPkgconfigDepends = (drv.libraryPkgconfigDepends or [ ]) ++ gtkDeps;
              executableSystemDepends = (drv.executableSystemDepends or [ ]) ++ gtkDeps;
              executablePkgconfigDepends = (drv.executablePkgconfigDepends or [ ]) ++ gtkDeps;
              testSystemDepends = (drv.testSystemDepends or [ ]) ++ gtkDeps;
              testPkgconfigDepends = (drv.testPkgconfigDepends or [ ]) ++ gtkDeps;
            }) (hsuper.callCabal2nix "chess-hs" ./. { });
          };
        };
      in
      {
        packages = {
          default = myHaskellPackages.chess-hs;
          chess-hs = myHaskellPackages.chess-hs;
        };

        # Dev environment for building the project
        devShells.default = pkgs.mkShell {
          # Packages needed for development
          buildInputs = with pkgs; [
            # Core dev tools
            myHaskellPackages.ghc
            cabal-install
            pkg-config

            # GTK dependencies
            gtk3
            gtk3.dev
            glib
            glib.dev
            gobject-introspection
            cairo
            cairo.dev
            pango
            pango.dev
          ];

          # Environment variables for GTK development
          shellHook = ''
            export PKG_CONFIG_PATH="${pkgs.lib.makeSearchPath "lib/pkgconfig" gtkDeps}:$PKG_CONFIG_PATH"
            export GI_TYPELIB_PATH="${pkgs.lib.makeSearchPath "lib/girepository-1.0" gtkDeps}:$GI_TYPELIB_PATH"
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath gtkDeps}:$LD_LIBRARY_PATH"
            export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share:${pkgs.gtk3}/share:$XDG_DATA_DIRS"

            echo "Haskell Chess GTK Development Environment"
            echo "Remember to use 'cabal build' inside this shell"
          '';
        };
      }
    );
}
