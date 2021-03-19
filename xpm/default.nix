{ ... }:

{
  primary-user.home-manager = {
    home.file = {
      ".config/xmobar/xpm" = {
        source = ./xpm;
        recursive = true;
      };
    };
  };
}
