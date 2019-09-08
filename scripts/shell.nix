let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  name="scripts";
  inputsFrom=[
    (import ./release.nix).project.env
  ];
  buildInputs=[
    (pkgs.writeWatchScript {
      name = "watch-all";
      src = "."; 
      exclude = "dist-newstyle";
      command = "cabal new-build all";
    })
  ];
}

