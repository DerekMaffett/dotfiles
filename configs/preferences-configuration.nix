# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

with lib;

let 

in {
  # Enable the X11 windowing system.
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.xkbOptions = "caps:ctrl_modifier";

  services.xserver.exportConfiguration = true;

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
