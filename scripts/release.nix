let 
  pkgs = import <nixpkgs-unstable> {};

in
{
  project = pkgs.haskellPackages.callPackage ./project.nix {};
}
