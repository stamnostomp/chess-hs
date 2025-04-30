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
        pkgs = nixpkgs.legacyPackages.${system};

        # Create a Haskell environment
        myHaskellPackages = pkgs.haskellPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell development
            myHaskellPackages.ghc
            cabal-install

            # System tools
            pkg-config

            # GTK and dependencies
            gtk3
            glib
            gobject-introspection
            pango
            cairo

            # Development tools
            haskell-language-server
          ];

          # Shell hook with environment variables
          shellHook = ''
            echo "Haskell Chess GTK Development Shell"

            # Make pkg-config find the GTK libraries
            export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${pkgs.gtk3.dev}/lib/pkgconfig"
            export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${pkgs.glib.dev}/lib/pkgconfig"
            export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${pkgs.pango.dev}/lib/pkgconfig"
            export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${pkgs.cairo.dev}/lib/pkgconfig"

            # Set GI repository path
            export GI_TYPELIB_PATH="$GI_TYPELIB_PATH:${pkgs.gtk3}/lib/girepository-1.0"
            export GI_TYPELIB_PATH="$GI_TYPELIB_PATH:${pkgs.glib}/lib/girepository-1.0"
            export GI_TYPELIB_PATH="$GI_TYPELIB_PATH:${pkgs.pango}/lib/girepository-1.0"
            export GI_TYPELIB_PATH="$GI_TYPELIB_PATH:${pkgs.cairo}/lib/girepository-1.0"

            # Set XDG_DATA_DIRS for themes and schemas
            export XDG_DATA_DIRS="$XDG_DATA_DIRS:${pkgs.gtk3}/share"
            export XDG_DATA_DIRS="$XDG_DATA_DIRS:${pkgs.gsettings-desktop-schemas}/share"
          '';
        };
      }
    );
}
