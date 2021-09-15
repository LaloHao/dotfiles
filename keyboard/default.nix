{ config, options, lib, pkgs, ... }:

with lib; with builtins;

let
  cfg = config.services.kmonad;

  mkService = kbd-dev: kbd-path:
    let
      dev = "dev-input-${kbd-dev}.device";
    in {
      name = "kmonad-${kbd-dev}";
      value = {
        Unit =
          { Description = "KMonad Instance for: ${kbd-dev}";
            BindsTo = dev;
            After   = dev;
            Requisite = dev;
          };
        Service =
          { Type = "simple";
            Restart = "always";
            RestartSec = 10;
            ExecStart = "${cfg.package}/bin/kmonad ${kbd-path}"; };
        Install =
          { WantedBy = [ dev ]; };
      };
    };

in {
  options.services.kmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to enable KMonad service.
      '';
    };

    devices = mkOption {
      type = types.attrsOf types.path;
      default = {};
      example = ''
        { "K480" = ./my-config.kbd; }
      '';
      description = ''
        Input devices mapped to their respective configuration file.
      '';
    };

    package = mkOption {
      type = types.package;
      default = import ../kmonad.nix {};
      example = "import ./kmonad.nix";
      description = ''
        The kmonad package.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    systemd.user.services = listToAttrs (mapAttrsToList mkService cfg.devices);
  };
}
