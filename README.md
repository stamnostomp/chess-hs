# Chess-HS

A chess game implemented in Haskell with both terminal and GTK interfaces.

## Features

- Play chess with a text-based terminal interface
- Play chess with a GTK graphical interface
- Standard chess rules and move validation
- Move history tracking
- Check, checkmate, and stalemate detection

## Requirements

The easiest way to set up the development environment is using Nix with flakes enabled:

```bash
# Enable flakes if you haven't already
echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf

# Enter the development shell
nix develop
```

Alternatively, you can install the dependencies manually:

- GHC (Glasgow Haskell Compiler) 9.0 or later
- Cabal 3.0 or later
- GTK3 development libraries
- Cairo, Pango, and GLib development libraries

## Building

Using Nix:

```bash
nix build
```

Using Cabal:

```bash
cabal build
```

## Running

To run the terminal interface:

```bash
cabal run
```

To run the GTK interface:

```bash
cabal run -- --gtk
```

## Development

This project uses:

- A Nix flake for reproducible builds and development environments
- Haskell Language Server for IDE support
- GHC for compilation
- Cabal for package management

### Directory Structure

- `app/` - Main executable
- `src/` - Library code
  - `Chess/Board.hs` - Chess board representation
  - `Chess/Pieces.hs` - Chess piece definitions
  - `Chess/Game.hs` - Game state and logic
  - `Chess/Move.hs` - Move representation and execution
  - `Chess/Rules.hs` - Chess rules and move validation
  - `Chess/UI/` - User interfaces
    - `Common.hs` - Shared UI utilities
    - `Terminal.hs` - Terminal-based UI
    - `GTK.hs` - GTK-based graphical UI
- `test/` - Test suite

## License

MIT License (see LICENSE file)
