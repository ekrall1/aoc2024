{
  description = "A flake for building the aoc2024 project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };

      ocamlPackages = pkgs.ocamlPackages;
    in
    {

      # packages that can be run for development:
      #
      #   $ nix develop
      #
      devShells.${pkgs.system}.default = pkgs.mkShell {
        buildInputs =
          with pkgs;
          [
            dune_3
            gnumake
            opam
          ]
          ++ (with ocamlPackages; [
            ocaml
            base
            stringext
            utop
            ocamlformat_0_26_2
            ocp-indent
            ocaml-lsp
            re
            ppx_jane
          ]);

        shellHook = ''
          echo "Entering development shell"
          export DUNE_BUILD_DIR=./result/_build
          source ./alias.sh
        '';
      };

      packages = {
        "${pkgs.system}" = {
          # package run by default with
          #   $ nix build
          #
          default = self.packages.${pkgs.system}.aoc2024;

          # packages can be run individually with
          #   $ nix build #.<name>
          #
          aoc2024 = ocamlPackages.buildDunePackage {
            pname = "aoc2024";
            version = "0.0.1";
            src = "./.";
            duneVersion = "3";

            minimalOcamlVersion = "5.2";

            buildInputs = with ocamlPackages; [
              ocaml
              base
              stringext
              re
              ppx_jane
            ];

            unpackPhase = "true";

            buildPhase = ''
              dune build \
              --root="${./.}" \
              --build-dir="$(pwd)/_build" \
              --only-packages=aoc2024
            '';

            installPhase = ''
              mkdir -p $out/_build
              cp -r $(pwd)/_build/* $out/_build/
            '';

          };
        };
      };

      #   run checks
      #   $ nix flake check
      #
      #   or,
      #   $ nix build .#checks.x86_64-linux.<check name>
      #
      checks = {
        "${pkgs.system}" = {

          aoc2024-test = self.packages.${pkgs.system}.aoc2024.overrideAttrs ({
            name = "dune-runtest";
            doCheck = true;
            checkPhase = ''

              dune build \
              --root="${./.}" \
              --build-dir="$(pwd)/_build"

              echo "running tests"

              dune runtest \
              --root="${./.}" \
              --build-dir="$(pwd)/_build"
            '';
          });

          aoc2024-format =
            nixpkgs.legacyPackages.${pkgs.system}.runCommand "aoc2024-format"
              {
                nativeBuildInputs = [
                  nixpkgs.legacyPackages.${pkgs.system}.ocamlPackages.dune_3
                  nixpkgs.legacyPackages.${pkgs.system}.ocamlPackages.ocaml
                  nixpkgs.legacyPackages.${pkgs.system}.ocamlPackages.ocamlformat_0_26_2
                ];
              }
              ''
                dune fmt \
                --root="${./.}" \
                --build-dir="$(pwd)/_build"
                touch $out
              '';
        };
      };
    };
}
