{
  description = "chess-hs: A terminal-based chess game in Haskell with GTK interface";

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

        # Haskell package with dependencies
        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            # You can add Haskell package overrides here if needed
          };
        };

        # Project dependencies
        packageDeps = with pkgs; [
          # GTK dependencies
          gtk3
          gtksourceview4
          gobject-introspection
          glib
          cairo
          pango
          gdk-pixbuf

          # Build tools
          pkg-config
          cabal-install
          ghc
        ];

        # Development shell packages
        devPackages = with pkgs; [
          haskell-language-server
          hlint
          haskellPackages.cabal-fmt
          haskellPackages.ghcid
          haskellPackages.implicit-hie
        ];

      in
      {
        packages.default = haskellPackages.callCabal2nix "chess-hs" ./. { };

        devShells.default = pkgs.mkShell {
          buildInputs = packageDeps ++ devPackages;

          # Environment variables
          shellHook = ''
            echo "Haskell Chess GTK Development Shell"
            export GI_TYPELIB_PATH=${pkgs.gtk3}/lib/girepository-1.0:${pkgs.gtksourceview4}/lib/girepository-1.0:${pkgs.gdk-pixbuf}/lib/girepository-1.0:${pkgs.pango.out}/lib/girepository-1.0
            export XDG_DATA_DIRS=${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS
          '';
        };
      }
    );
}
