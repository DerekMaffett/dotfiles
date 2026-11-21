{
  description = "NixOS machines built from these dotfiles";

  # Same branches as configs/.nix-channels, so the Ubuntu setup and NixOS
  # get the same package versions.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }: {
    # Apply with: sudo nixos-rebuild switch --flake ~/dotfiles#derek
    nixosConfigurations.derek = nixpkgs.lib.nixosSystem {
      modules = [
        home-manager.nixosModules.home-manager
        ./nixos/hosts/derek/configuration.nix
      ];
    };
  };
}
