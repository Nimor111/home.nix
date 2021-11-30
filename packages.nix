{pkgs, ...}:

{
  packages = [
    #pkgs.scala

    pkgs.imagemagick
    pkgs.fd
    pkgs.sbt
    pkgs.jdk11
    pkgs.gcc
    pkgs.ammonite
    pkgs.slack
    pkgs.nodejs-12_x
    pkgs.xclip
    pkgs.xdotool
    pkgs.xautolock
    pkgs.gopass
    pkgs.feh
    pkgs.zathura
    pkgs.exa
    pkgs.bat
    pkgs.direnv
    pkgs.pfetch
    pkgs.ripgrep
    pkgs.termonad-with-packages
    pkgs.xmobar
    pkgs.dmenu
    pkgs.trayer
    pkgs.nextcloud-client
    pkgs.cmake
    pkgs.nix-prefetch-git
    pkgs.navi
    pkgs.thefuck
    pkgs.clojure
    pkgs.sqlite
    pkgs.brave
    pkgs.mu
    pkgs.ledger
    #pkgs.python3
    pkgs.python310
    pkgs.scrot
    pkgs.tree
    pkgs.jq

    (let neuronRev = "44855fb8674e74a6b9a6688a8dff0298e9c78124";
        neuronSrc = builtins.fetchTarball "https://github.com/srid/neuron/archive/${neuronRev}.tar.gz";
     in import neuronSrc {})

    (pkgs.callPackage ./pkgs/tmux-up {})
  ];
}
