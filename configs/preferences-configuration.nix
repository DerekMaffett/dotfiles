# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

with lib;

let 

in {
  imports = [ <home-manager/nixos> ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # Enable the X11 windowing system.
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.xkbOptions = "caps:ctrl_modifier";

  services.xserver.exportConfiguration = true;

  # Enable Docker
  virtualisation.docker.enable = true;
  users.users.derek.extraGroups = [ "docker" ];

  users.users.derek.isNormalUser = true;
  home-manager.users.derek = { pkgs, ... }: {
    home.stateVersion = "22.11";
    home.packages = [ 
      pkgs.vim 
      pkgs.kitty 
      pkgs.git 
      pkgs.curl 
      pkgs.home-manager 
    ];
  };

  nixpkgs.overlays = [
    (self: super: {
      fcitx-engines = pkgs.fcitx5;
    })
  ];

  # programs.sway.enable = true;

  # xdg = {
  #   portal = {
  #     enable = true;
  #     extraPortals = with pkgs; [
  #       xdg-desktop-portal-wlr
  #       xdg-desktop-portal-gtk
  #     ];
  #     gtkUsePortal = true;
  #   };
  # };

  # DISABLED: Using dconf directly instead...

  # services.xserver.libinput = {
  #   enable = true;
  #   touchpad.tapping = true;
  #   touchpad.naturalScrolling = true;
  #   touchpad.scrollMethod = "twofinger";
  # };

#   services.xserver.windowManager.xmonad = {
#    enable = true;
#    enableContribAndExtras = true;
#    config = builtins.readFile ./xmonad.hs;
#  };
}
