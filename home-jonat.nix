{ config, pkgs, ... }:
rec {

  home.username = "jonat";
  home.homeDirectory = "/home/jonat";

  home.packages = [ pkgs.htop ];

  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "emacs";
    };
    shellAliases = {
      l = "ls -a --color=auto";
      lss = "ls -hAlt -gG --color=auto";
      nrs = "sudo nixos-rebuild switch -I nixos-config=${home.homeDirectory}/nixos-dotfiles/configuration.nix";
    };
  };

  programs.git = {
    enable = true;
    userEmail = "jonathanfung2000@gmail.com";
    userName = "Jonathan Fung";
  };

  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      output = {
        "*" = {
          bg = "#a5b2bf solid_color";
        };
        # Framework Laptop Screen
        "eDP-1" = {
          resolution = "2256x1504";
          position = "208,1080";
          scale = "1.5";
        };

        # Philips 4k
        "DP-9" = {
          resolution = "3840x2160";
          position = "0,0";
          scale = "2.0";
        };
      };

      ################################################
      modifier = "Mod4";
      floating.modifier = modifier;
      left = "h";
      down = "j";
      up = "k";
      right = "l";
      menu = "fuzzel";

      input = {
        "type:keyboard" = {
          repeat_delay = "200";
          repeat_rate = "40";
        };
      };

      keybindings = {
        "${modifier}+Return" = "exec ${pkgs.foot}/bin/foot";
        "${modifier}+Shift+Return" = "exec ${pkgs.foot}/bin/foot -a 'floating'";
        "${modifier}+Shift+q" = "kill";

        "${modifier}+d" = "exec ${pkgs.fuzzel}/bin/fuzzel";
        "${modifier}+Shift+d" = "exec ./toggle_menu.sh"; # TODO

        XF86AudioMute = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";
        XF86AudioLowerVolume = "exec pactl set-sink-volume @DEFAULT_SINK@ -2%";
        XF86AudioRaiseVolume = "exec pactl set-sink-volume @DEFAULT_SINK@ +2%";
        XF86AudioPlay = "exec playerctl -p spotify play-pause";
        XF86AudioPrev = "exec playerctl -p spotify previous";
        XF86AudioNext = "exec playerctl -p spotify next";

        XF86MonBrightnessDown = "exec brightnessctl s 1%-";
        XF86MonBrightnessUp = "exec brightnessctl s 1%+";

        "${modifier}+Shift+r" = "reload";
        "${modifier}+Shift+e" = "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -B 'Yes, exit sway' 'swaymsg exit'";

        "${modifier}+${left}" = "focus left";
        "${modifier}+${down}" = "focus down";
        "${modifier}+${up}" = "focus up";
        "${modifier}+${right}" = "focus right";
        "${modifier}+Shift+${left}" = "move left";
        "${modifier}+Shift+${down}" = "move down";
        "${modifier}+Shift+${up}" = "move up";
        "${modifier}+Shift+${right}" = "move right";

        "${modifier}+1" = "workspace number 1";
        "${modifier}+2" = "workspace number 2";
        "${modifier}+3" = "workspace number 3";
        "${modifier}+4" = "workspace number 4";
        "${modifier}+5" = "workspace number 5";
        "${modifier}+6" = "workspace number 6";
        "${modifier}+7" = "workspace number 7";
        "${modifier}+8" = "workspace number 8";
        "${modifier}+9" = "workspace number 9";
        "${modifier}+0" = "workspace number 10";

        "${modifier}+Shift+1" = "move container to workspace number 1";
        "${modifier}+Shift+2" = "move container to workspace number 2";
        "${modifier}+Shift+3" = "move container to workspace number 3";
        "${modifier}+Shift+4" = "move container to workspace number 4";
        "${modifier}+Shift+5" = "move container to workspace number 5";
        "${modifier}+Shift+6" = "move container to workspace number 6";
        "${modifier}+Shift+7" = "move container to workspace number 7";
        "${modifier}+Shift+8" = "move container to workspace number 8";
        "${modifier}+Shift+9" = "move container to workspace number 9";
        "${modifier}+Shift+0" = "move container to workspace number 10";

        "${modifier}+b" = "splith";
        "${modifier}+v" = "splitv";
        "${modifier}+s" = "layout stacking";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+f" = "fullscreen";

        "${modifier}+space" = "focus mode_toggle";
        "${modifier}+Shift+space" = "floating toggle";
        "${modifier}+a" = "focus parent";

        "${modifier}+minus" = "scratchpad show";
        "${modifier}+Shift+minus" = "move scratchpad";
        "${modifier}+tab" = "workspace back_and_forth";
        "${modifier}+Shift+v" = "move workspace to output up";
        "${modifier}+Shift+c" = "move workspace to output right";

        "${modifier}+ctrl+Return" = "exec emacs";
      };

      ################################################
      floating.criteria = [
        { app_id = "floating"; }
        { title = "floating"; }
      ];
      colors.focused = {
        border = "#ffffff";
        background = "#ffffff";
        text = "#000000";
        indicator = "#80d2e0";
        childBorder = "#00bfe0";
      };
      gaps = {
        inner = 10;
        horizontal = 6;
        top = 6;
        bottom = -10;
      };
      bars = [ { command = "${pkgs.waybar}/bin/waybar"; } ];
    };
    # TODO Use iosevka.privateBuildPlan.family
    extraConfig = ''
      default_border pixel 3
      title_align center
      font "Iosevka Custom" 9
    '';
  };

  programs.waybar = {
    enable = true;
    settings.mainBar = {
      position = "bottom";
      modules-left = [
        "sway/workspaces"
        "sway/mode"
        "pulseaudio"
        "backlight"
        "custom/spotify"
      ];
      modules-center = [
        "idle_inhibitor"
        "clock"
      ];
      modules-right = [
        "cpu"
        "memory"
        "temperature"
        "battery"
        "network"
        "tray"
      ];

      "sway/mode" = {
        format = "<span style=\"italic\">{}</span>";
      };

      pulseaudio = {
        ignored-sinks = [ "Easy Effects Sink" ]; # https://github.com/Alexays/Waybar/pull/1636
        format = "V: {volume}%";
        format-bluetooth = "V: {volume}%  {format_source}";
        format-bluetooth-muted = " {icon} {format_source}";
        format-muted = " ";
        format-source = "{volume}% ";
        format-source-muted = "";
        format-icons = {
          headphone = "";
          hands-free = "";
          headset = "";
          phone = "";
          portable = "";
          car = "";
          default = [
            ""
            ""
            ""
          ];
        };
        on-click = "pavucontrol";
      };
      backlight.format = "D: {percent}%";

      "custom/spotify" = {
        format = "{icon} {}";
        escape = true;
        max-length = 40;
        interval = 15;
        on-click = "playerctl -p spotify play-pause";
        smooth-scrolling-threshold = 10;
        on-scroll-up = "playerctl -p spotify next";
        on-scroll-down = "playerctl -p spotify previous";
        exec = "echo $(playerctl -p spotify metadata artist) - $(playerctl -p spotify metadata title)";
        exec-if = "pgrep spotify";
      };
      # https://github.com/Alexays/Waybar/issues/690
      # https://github.com/Alexays/Waybar/issues/1237
      idle_inhibitor = {
        format = "{icon}";
        format-icons = {
          activated = "█";
          deactivated = "░";
        };
      };
      clock = {
        timezone = "America/Los_Angeles";
        format = "{:%a %b %d Ξ %I:%M %p}";
        tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        format-alt = "{:%Y-%m-%d}";
      };
      ##################################################
      cpu.format = "C: {usage}%";

      memory.format = "M: {}%";
      temperature = {
        # hwmon-path = "/sys/class/thermal/thermal_zone6/temp"; # 241128: zone 6 is from old intel framework
        critical-threshold = 80;
        format = "T: {temperatureC}°C";
      };
      battery = {
        states = {
          warning = 30;
          critical = 30;
        };
        format = "B: {capacity}%";
        format-charging = "B: {capacity}%";
        format-plugged = "B: {capacity}%";
        format-alt = "{time}";
      };
      network = {
        format-wifi = "{essid} ({signalStrength}%)";
        format-ethernet = "{ipaddr}/{cidr}";
        tooltip-format = "{ifname} via {gwaddr}";
        format-linked = "{ifname} (No IP)";
        format-disconnected = "Disconnected ⚠";
        format-alt = "{ifname}: {ipaddr}/{cidr}";
      };
      tray = {
        icon-size = 21;
        spacing = 10;
      };
    };

    style = ''
      * {
          /* `otf-font-awesome` is required to be installed for icons */
          font-family: Iosevka Custom, FontAwesome, Roboto, Helvetica, Arial, sans-serif;
          font-size: 10px;
      }

      window#waybar {
          /* background-color: rgba(43, 48, 59, 1.0); */
          background-color: rgba(0, 0, 0, 0.0);
          /* border-bottom: 3px solid rgba(100, 114, 125, 0.5); */
          color: #ffffff;
          transition-property: background-color;
          transition-duration: .5s;
          /* margin: 0; */
          /* padding: 0; */
      }
      .modules-left{
          background-color: #707070;
          /* border-top: #707070 3px solid; */
          border-right: #707070 3px solid;
      }

      .modules-center{
          background-color: #707070;
          /* border-top: #707070 3px solid; */
          border-left: #707070 3px solid;
          border-right: #707070 3px solid;
      }

      .modules-right{
          background-color: #707070;
          /* border-top: #707070 3px solid; */
          border-left: #707070 3px solid;
      }

      /* ******************************************  */

      window#waybar.hidden {
          opacity: 0.2;
      }

      /*
      window#waybar.empty {
          background-color: transparent;
      }
      window#waybar.solo {
          background-color: #FFFFFF;
      }
      */

      /* window#waybar.termite { */
      /*     background-color: #3F3F3F; */
      /* } */

      /* window#waybar.chromium { */
      /*     background-color: #000000; */
      /*     border: none; */
      /* } */

      #workspaces button {
          padding: 0 2px;
          background-color: transparent;
          color: white;
          font-weight: bold;
          /* Use box-shadow instead of border so the text isn't offset */
          /* box-shadow: inset 0 -3px transparent; */
          /* Avoid rounded borders under each workspace name */
          border: none;
          border-radius: 0;
      }

      /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
      #workspaces button:hover {
          background: rgba(0, 0, 0, 0.2);
          box-shadow: inset 0 -3px #ffffff;
      }

      /* displayed, but not focues (different monitor) */
      /* Must be written before button.focused */
      #workspaces button.visible {
          background-color: white;
          color: black;
          /* box-shadow: inset 0 -3px #ffffff; */
      }

      #workspaces button.focused {
          background-color: white;
          color: #00BFE0;
          /* box-shadow: inset 0 -3px #ffffff; */
      }

      #workspaces button.urgent {
          background-color: #eb4d4b;
      }

      #mode {
          background-color: #64727D;
          border-bottom: 3px solid #ffffff;
      }

      #clock,
      #battery,
      #cpu,
      #memory,
      #disk,
      #temperature,
      #backlight,
      #network,
      #pulseaudio,
      #custom-media,
      #tray,
      #mode,
      #idle_inhibitor,
      #mpd,
      #custom-spotify,
      /* #custom-sptlrx, */
      #custom-wttr {
          padding: 4px 4px;
          background-color: white;
          color: #000000;
      }

      #window,
      #workspaces {
          margin: 0;
      }

      /* If workspaces is the leftmost module, omit left margin */
      .modules-left > widget:first-child > #workspaces {
          margin-left: 0;
      }

      /* If workspaces is the rightmost module, omit right margin */
      .modules-right > widget:last-child > #workspaces {
          margin-right: 0;
      }

      #clock {
          /* box-shadow: inset 0 -3px #64727D; */
      }

      #battery {
          box-shadow: inset 0 -3px #ffffff;
      }

      #battery.charging, #battery.plugged {
          box-shadow: inset 0 -3px #26A65B;
      }

      @keyframes blink {
          to {
          box-shadow: inset 0 -3px #ffffff;
              color: #000000;
          }
      }

      #battery.critical:not(.charging) {
          box-shadow: inset 0 -3px #f53c3c;
          animation-name: blink;
          animation-duration: 2.0s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
      }

      label:focus {
          background-color: #000000;
      }

      #cpu {
          box-shadow: inset 0 -3px #2ecc71;
      }

      #memory {
          box-shadow: inset 0 -3px #9b59b6;
      }

      #disk {
          background-color: #964B00;
      }

      #backlight {
          box-shadow: inset 0 -3px #90b1b1;
      }

      #network {
          box-shadow: inset 0 -3px #2980b9;
      }

      #network.disconnected {
          box-shadow: inset 0 -3px #f53c3c;
      }

      #pulseaudio {
          box-shadow: inset 0 -3px #f1c40f,
                      inset 3px 0 #707070;
          padding-left: 6px;
      }

      #pulseaudio.muted {
          box-shadow: inset 0 -3px #90b1b1;
      }

      #custom-spotify {
          box-shadow: inset 0 -3px #1DB954;
      }

      /* #custom-sptlrx { */
      /*     font-size: 12px; */
      /* } */

      #custom-media {
          background-color: #66cc99;
          color: #2a5c45;
          min-width: 100px;
      }

      #custom-media.custom-spotify {
          background-color: #66cc99;
      }

      #custom-media.custom-vlc {
          background-color: #ffa000;
      }

      #temperature {
          box-shadow: inset 0 -3px #f0932b;
      }

      #temperature.critical {
          box-shadow: inset 0 -3px #eb4d4b;
      }

      #tray {
          background-color: #2980b9;
      }

      #tray > .passive {
          -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: #eb4d4b;
      }

      #idle_inhibitor {
          /* background-color: #2d3436; */
      }

      #idle_inhibitor.activated {
          /* background-color: #ecf0f1; */
          /* color: #2d3436; */
      }

      #mpd {
          background-color: #66cc99;
          color: #2a5c45;
      }

      #mpd.disconnected {
          background-color: #f53c3c;
      }

      #mpd.stopped {
          background-color: #90b1b1;
      }

      #mpd.paused {
          background-color: #51a37a;
      }

      #language {
          background: #00b093;
          color: #740864;
          padding: 0 5px;
          margin: 0 5px;
          min-width: 16px;
      }

      #keyboard-state {
          background: #97e1ad;
          color: #000000;
          padding: 0 0px;
          margin: 0 5px;
          min-width: 16px;
      }

      #keyboard-state > label {
          padding: 0 5px;
      }

      #keyboard-state > label.locked {
          background: rgba(0, 0, 0, 0.2);
      }
    '';
  };
  programs.foot.enable = true;
  programs.foot.settings = {
    main = {
      font = "Iosevka Custom:size=14";
    };

    colors = {
      foreground = "ffffff";
      background = "000000";

      # modus-vivendi
      # https://github.com/kovidgoyal/kitty-themes/pull/13/files
      ## Normal/regular colors (color palette 0-7)
      regular0 = "000000"; # black
      regular1 = "ff8059"; # red
      regular2 = "44bc44"; # green
      regular3 = "d0bc00"; # yellow
      regular4 = "2fafff"; # blue
      regular5 = "feacd0"; # magenta
      regular6 = "00d3d0"; # cyan
      regular7 = "bfbbff"; # white

      ## Bright colors (color palette 8-15)
      bright0 = "595959"; # bright black
      bright1 = "ef8b50"; # bright red
      bright2 = "70b900"; # bright green
      bright3 = "c0c530"; # bright yellow
      bright4 = "79a8ff"; # bright blue
      bright5 = "b6a0ff"; # bright magenta
      bright6 = "6ae4b9"; # bright cyan
      bright7 = "ffffff"; # bright white
    };
  };

  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        font = "Iosevka Custom:size=12";
        terminal = "${pkgs.foot}/bin/foot -e";
        prompt = "'>>= '";
        anchor = "top";
        lines = 10;
        width = 40;
        vertical-pad = 20;
      };
      colors = {
        background = "ffffffff";
        text = "707070ff";
        prompt = "000000ff";
        input = "000000ff";
        match = "ff7c00ff";
        selection = "00bfe0ff";
        selection-text = "ffffffff";
        selection-match = "ff7c00ff";
        border = "707070ff";
      };
      border = {
        width = 4;
        radius = 0;
      };
    };
  };

  services.gammastep = {
    enable = true;
    latitude = 34.03;
    longitude = -118.15;
  };

  home.stateVersion = "24.11";
  programs.home-manager.enable = true;
}
