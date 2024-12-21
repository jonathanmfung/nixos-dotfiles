# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    <home-manager/nixos>
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices."luks-7b24a3c6-50bb-4e02-af2a-08e3219829c4".device = "/dev/disk/by-uuid/7b24a3c6-50bb-4e02-af2a-08e3219829c4";
  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  ###############################################################

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.jonat = {
    isNormalUser = true;
    description = "Jonathan Fung";
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
    ];
    packages = with pkgs; [ ];
  };

  home-manager.users.jonat = import ./home-jonat.nix {
    inherit config;
    inherit pkgs;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  programs.sway.enable = true;
  xdg.portal.wlr.enable = true;

  environment.systemPackages = with pkgs; [
    # Dev
    vim
    git

    # Langs
    sbcl
    python3

    # Tooling
    gnumake
    cmake
    clang-tools
    nixfmt-rfc-style
    poetry

    # Utils - CLI
    ffmpeg-full
    ispell
    ripgrep
    dunst
    brightnessctl
    playerctl
    gammastep
    yt-dlp
    texliveFull

    # Utils - GUI
    imv
    mpd
    mpv
    waybar
    fuzzel

    # Programs
    pavucontrol
    wdisplays
    zathura
    firefox
    spotify
    zoom-us
    syncthing
    # https://retorque.re/zotero-better-bibtex/installation/
    # https://github.com/syt2/zotero-scipdf
    zotero_7
  ];

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    (iosevka.override {
      privateBuildPlan = {
        # TODO: Rename to Iosevka SS14?
        family = "Iosevka Custom";
        spacing = "normal";
        serifs = "sans";
        noCvSs = true;
        exportGlyphNames = false;
        variants = {
          inherits = "ss14";
        };
      };
      set = "SS14";
    })
    # TODO: Add privateBuildPlan for Iosevka Etoile
    # [buildPlans.IosevkaCustom]
    # family = "Iosevka Custom"
    # spacing = "quasi-proportional"
    # serifs = "slab"
    # noCvSs = true
    # exportGlyphNames = false
    # [buildPlans.IosevkaCustom.variants]
    # inherits = "ss14"
  ];

  nixpkgs.overlays = [
    (import (
      builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/7c0720d624aae2b9ab356e3e858a2490bce1b822.tar.gz";
      }
    ))

    # Custom Scripts
    (final: prev: {
      menu = (
        final.writeShellApplication (
          import ./scripts/menu.nix {
            inherit (final) fuzzel;
            inherit (final) wtype;
          }
        )
      );
    })
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:
  services.emacs.enable = true;
  services.emacs.package = (
    pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      defaultInitFile = true;
      package = pkgs.emacs-unstable-pgtk;
      alwaysEnsure = true;
      override =
        epkgs:
        epkgs
        // {
          coalton-mode = pkgs.callPackage ./elisp/coalton-mode.nix {
            inherit (pkgs) fetchFromGitHub;
            inherit (epkgs) trivialBuild;
          };
          consult-xref-stack = pkgs.callPackage ./elisp/consult-xref-stack.nix {
            inherit (pkgs) fetchFromGitHub;
            inherit (epkgs) trivialBuild;
            inherit (epkgs) consult;
          };
        };
    }
  );

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
