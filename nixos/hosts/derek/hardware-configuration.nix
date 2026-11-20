# PLACEHOLDER so the flake evaluates. On the real machine, replace this file
# with the one the installer writes (or `nixos-generate-config --show-hardware-config`):
# it holds that machine's disks, partitions and kernel modules.
{ lib, ... }:

{
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
