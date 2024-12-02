{ pkgs ? import <nixpkgs> {} }:

let
  libPath = with pkgs; lib.makeLibraryPath [];
  ocamlPackages = pkgs.recurseIntoAttrs pkgs.ocamlPackages;
in
  pkgs.mkShell rec {
    buildInputs = with pkgs; [
      dune_3
      gnumake
      opam
    ] ++ ( with ocamlPackages;
    [
      ocaml
      base
      stdio
      stringext
      utop
      ocamlformat
      ocp-indent
      ocaml-lsp
    ]);
  }
