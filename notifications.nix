{ config, pkgs, ... }:
{
  # https://github.com/phuhl/linux_notification_center
  home.packages = [
    pkgs.deadd-notification-center
  ];
}
