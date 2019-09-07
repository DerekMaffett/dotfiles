let
  pkgs = import <nixpkgs-unstable> {};
in
pkgs.mkShell {
  name="scripts";
  inputsFrom=[
    (import ./release.nix).project.env
  ];
  buildInputs=[
    pkgs.fswatch
    (pkgs.writeScriptBin "watch" "fswatch -r -o -l 0.2 ./nix-github/ | (while read; do cabal new-build nix-github; done)")
  ];
}

