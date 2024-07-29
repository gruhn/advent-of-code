{
  description = "";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = nixpkgs.legacyPackages.${system}; 
      in 
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            lean4
            cargo
            rust-analyzer-unwrapped

            # haskell.compiler.ghc964
            # haskell.packages.ghc964.haskell-language-server
            # haskell.packages.ghc964.cabal-install
            # haskell.packages.ghc964.implicit-hie
          ];
        };
      }
    );
}
