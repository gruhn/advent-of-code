{
  description = "Advent Of Code 2018 - Tools";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    # Convenience functions for writing flakes
    flake-utils.url = "github:numtide/flake-utils";  
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: 
      let
        # Legacy packages that have not been converted to flakes
        legacyPackages = nixpkgs.legacyPackages.${system};
        # OCaml packages available on nixpkgs
        ocamlPackages = legacyPackages.ocamlPackages;
        # Library functions from nixpkgs
        lib = legacyPackages.lib;      
      in
      {
        # packages = {
        #   hello = ocamlPackages.buildDunePackage {
        #     pname = "hello";
        #     version = "0.1.0";
        #     duneVersion = "3.6";
        #     src = sources.ocaml;

        #     buildInputs = [
        #       # Ocaml package dependencies needed to build go here.
        #     ];

        #     strictDeps = true;

        #     preBuild = ''
        #       dune build hello.opam
        #     '';
        #   };        
        # };

        devShells = {
          default = legacyPackages.mkShell {
            # Development tools
            packages = [
              legacyPackages.ocaml
              legacyPackages.dune_3
              # Source file formatting
              legacyPackages.nixpkgs-fmt
              legacyPackages.ocamlformat
              # For `dune build --watch ...`
              legacyPackages.fswatch
              # For `dune build @doc`
              ocamlPackages.odoc
              # OCaml editor support
              ocamlPackages.ocaml-lsp
              # Nicely formatted types on hover
              ocamlPackages.ocamlformat-rpc-lib
              # Fancy REPL thing
              ocamlPackages.utop
            ];

            # # Tools from packages
            # inputsFrom = [
            #   self.packages.${system}.hello
            # ];
          };
        };
      }
    );
}
