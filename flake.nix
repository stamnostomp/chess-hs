{
  description = "chess-hs: A chess game in Haskell with GTK and terminal interfaces";

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
          gobject-introspection
        ];

        # Create a modified version of the Haskell packages
        myHaskellPackages = pkgs.haskellPackages.extend (
          self: super: {
            # Define a terminal-only version
            chess-hs-terminal = self.callCabal2nix "chess-hs" ./. {
              # Add flag to disable GTK
              configureFlags = [ "--flag=-enable-gtk" ];
            };

            # Define full version with explicit dependencies
            chess-hs-full = hlib.addBuildDepends (self.callCabal2nix "chess-hs" ./. { }) [
              self.haskell-gi-base
              self.gi-gtk
              self.gi-gdk
              self.gi-cairo
              self.gi-cairo-render
              self.gi-cairo-connector # Add the connector package
              self.gi-pango
              self.cairo # Add the traditional cairo package
              self.transformers # Add transformers for Control.Monad.Trans.Reader
            ];
          }
        );
      in
      {
        packages = {
          default = myHaskellPackages.chess-hs-full;
          full = myHaskellPackages.chess-hs-full;
          terminal = myHaskellPackages.chess-hs-terminal;
        };

        # Dev environment for building the project (full GTK version)
        devShells.default = pkgs.mkShell {
          # Packages needed for development
          buildInputs = with pkgs; [
            # Core dev tools
            ghc
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

            # Haskell packages for GTK
            haskellPackages.haskell-gi-base
            haskellPackages.gi-gtk
            haskellPackages.gi-gdk
            haskellPackages.gi-cairo
            haskellPackages.gi-cairo-render
            haskellPackages.gi-cairo-connector
            haskellPackages.gi-pango
            haskellPackages.cairo
            haskellPackages.transformers
          ];

          # Environment variables for GTK development
          shellHook = ''
            # Set library paths for GTK
            export PKG_CONFIG_PATH="${pkgs.lib.makeSearchPath "lib/pkgconfig" gtkDeps}:$PKG_CONFIG_PATH"
            export GI_TYPELIB_PATH="${pkgs.lib.makeSearchPath "lib/girepository-1.0" gtkDeps}:$GI_TYPELIB_PATH"
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath gtkDeps}:$LD_LIBRARY_PATH"
            export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share:${pkgs.gtk3}/share:$XDG_DATA_DIRS"

            # Create cabal.project.local with direct dependency references
            cat > cabal.project.local <<EOF
            package chess-hs
              flags: +enable-gtk
            EOF

            echo "Haskell Chess GTK Development Environment"
            echo "----------------------------------------"
            echo "Build full version:     nix build .#full"
            echo "Build terminal version: nix build .#terminal"
            echo "Run after building:     ./result/bin/chess-hs [--gtk]"
            echo ""
            echo "For development:"
            echo "  cabal build"
            echo "  cabal run -- [--gtk]"
          '';
        };

        # Terminal-only development environment
        devShells.terminal = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
          ];

          shellHook = ''
            # Create cabal.project.local without GTK
            cat > cabal.project.local <<EOF
            package chess-hs
              flags: -enable-gtk
            EOF

            echo "Haskell Chess Terminal-Only Development Environment"
            echo "---------------------------------------------------"
            echo "Build with:    nix build .#terminal"
            echo "Run after building: ./result/bin/chess-hs"
            echo ""
            echo "For development:"
            echo "  cabal build"
            echo "  cabal run"
          '';
        };
      }
    );
}
