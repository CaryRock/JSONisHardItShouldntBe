# https://docs.haskellstack.org/en/stable/topics/nix_integration/

{
  description = "BLMTER";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          hPkgs = pkgs.haskell.packages."ghc984"; # Might require bumping the nixpkgs.url version

          myDevTools = [
            hPkgs.ghc     # GHC compiler in the desired version 
            hPkgs.ghcid   # Continuous terminal Haskell compile checker
            hPkgs.ormolu  # Haskell formatter
            hPkgs.hlint   # Haskell codestyle checker
            hPkgs.hoogle  # Lookup Haskell documentation
            hPkgs.haskell-language-server # LSP server for editor
            hPkgs.implicit-hie  # Auto-generate LSP hie.yaml file from cabal
            hPkgs.retrie  # Haskell refactoring tool
            #hPkgs.cabal-install
            stack-wrapped # Defined below
            pkgs.zlib
            pkgs.pkg-config
          ];

          # Wrap Stack to work with our Nix integration. We do not want to
          # modify stack.yaml so that non-Nix users do not notice anything.
          # - no-nix          # We do not want Stack's way of integrating Nix.
          # --system-ghc      # Use the existing GHC on PATH (will come from this .nix file)
          # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack"; # will be available as the usual `stack` in the terminal
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };
  in {
    devShells.default = pkgs.mkShell {
      buildInputs = [
        myDevTools
        pkgs.hdf5
        #pkgs.hdf5-threadsafe
        pkgs.libffi
        pkgs.zlib
        pkgs.lz4
        pkgs.bzip2
      ];
  
      # Make external Nix C libraries like zlib known to GHC, like
      # pkgs.haskell.lib.buildSatckProject does
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
    };
  });
}
