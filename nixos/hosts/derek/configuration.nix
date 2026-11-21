# The whole machine: what the Ubuntu installer, bootstrap-ubuntu.sh and
# install-apps.sh set up there. Applied by bootstrap-nixos.sh.
{ lib, pkgs, ... }:

let
  user = "derek";

  # Web Store IDs from configs/chrome-extensions.txt, comments dropped.
  chromeExtensions = lib.pipe ../../../configs/chrome-extensions.txt [
    builtins.readFile
    (lib.splitString "\n")
    (map (line: lib.trim (lib.head (lib.splitString "#" line))))
    (lib.filter (id: id != ""))
  ];
in
{
  imports = [ ./hardware-configuration.nix ];

  # --- Boot, network, locale (the Ubuntu installer's choices) ---

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "derek";
  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Stockholm";
  i18n.defaultLocale = "en_US.UTF-8";

  # Swap to compressed RAM instead of disk: much faster than disk swap and
  # needs no partition. Any swap the installer set up (swapDevices in
  # hardware-configuration.nix) stays as a slower fallback behind it.
  zramSwap.enable = true;

  # --- Desktop ---

  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  # Caps Lock as Ctrl on the login screen and text consoles too. Inside the
  # GNOME session, dconf.settings in configs/home.nix already does this.
  services.xserver.xkb = {
    layout = "us";
    options = "ctrl:nocaps";
  };
  console.useXkbConfig = true;

  # --- Nix (replaces the single-user install and channels) ---

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  nixpkgs.config = import ../../../configs/nix-config.nix;

  # --- Users ---

  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
  };

  # --- Apps and services (install-apps.sh) ---

  virtualisation.docker.enable = true;

  environment.systemPackages = with pkgs; [
    google-chrome
    slack
    postman
    ghostty
    # TODO Claude Desktop: nixpkgs has no package, and Anthropic only ships a
    # .deb. Needs a community flake or a package written here.
  ];

  # Writes Chrome's managed policy. Unlike install-apps.sh, these are
  # force-installed: Chrome won't let you remove them, only this list can.
  programs.chromium = {
    enable = true;
    extensions = chromeExtensions;
  };

  # --- Your user environment (configs/home.nix) ---

  home-manager = {
    # Use this system's nixpkgs and its allowUnfree, instead of a second copy.
    useGlobalPkgs = true;
    # Install packages into ~/.nix-profile as on Ubuntu, so .bashrc and the
    # desktop-entry mirroring in home.nix work unchanged.
    useUserPackages = false;
    users.${user}.imports = [
      ../../../configs/home.nix
      ../../home.nix
    ];
  };

  # --- Test VM only ---
  # Applies only to `nix build .#nixosConfigurations.derek.config.system.build.vm`,
  # never to the real machine.
  virtualisation.vmVariant = {
    # Throwaway login for the VM. A real install sets its password in the
    # installer instead.
    users.users.${user}.initialPassword = "nixos-vm";

    virtualisation = {
      memorySize = 8192;
      cores = 4;
      diskSize = 20 * 1024;
      resolution = { x = 1920; y = 1080; };

      # nixos/home.nix links dotfiles to ~/dotfiles, so mount this repo there.
      # Edits on either side show up on the other.
      sharedDirectories.dotfiles = {
        source = "/home/${user}/dotfiles";
        target = "/home/${user}/dotfiles";
      };
    };
  };

  # The NixOS release this machine was first installed with. Set it once at
  # install time (the installer writes it) and don't change it afterwards.
  system.stateVersion = "26.05";
}
