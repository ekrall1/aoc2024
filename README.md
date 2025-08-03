*Advent of Code 2024*

Using OCaml on NixOS

Start development environment

    nix develop

Within the dev environment:

    nix build
    dune runtest (runs tests)
    utop (repl)

Get the solution for a specific day/part

    aoc2024 -day-of-advent 1 -part 1 -input "test_data/d1_test"

