{ config, pkgs, ... }:

let
  userInfo = import ./user.nix { };
  myPackages = (import ./packages.nix { inherit pkgs; }).packages;

  # TODO put custom vim plugins in another file
  # nix-prefetch-git <git-url>
  # plugin-name = pkgs.vimUtils.buildVimPlugin {
  # name = "plugin-name";
  # src = pkgs.fetchFromGitHub {
  # owner = "plugin-org";
  # repo = "plugin-repo";
  # rev = "<ref>";
  # sha256 = "<sha256>";
  # };
  # };

  vim-zettel = pkgs.vimUtils.buildVimPlugin {
    name = "vim-zettel";
    src = pkgs.fetchFromGitHub {
      owner = "michal-h21";
      repo = "vim-zettel";
      rev = "929d90eec62e6f693c2702d2b6f76a93f2f1689d";
      sha256 = "07ma6ylvvyncr24pinvlybygddjdi2r835x7q8c52mnz96dcmz6m";
    };
  };

  vim-which-key = pkgs.vimUtils.buildVimPlugin {
    name = "vim-which-key";
    src = pkgs.fetchFromGitHub {
      owner = "liuchengxu";
      repo = "vim-which-key";
      rev = "2c915b6de918c073fbd83809e51343651f00f9a8";
      sha256 = "05jdjmpyczcgqsm5mznyb79bq10ianv7v3rhxy9wrklkama3jrgs";
    };
  };

  vim-gtd = pkgs.vimUtils.buildVimPlugin {
    name = "gtd.vim";
    src = pkgs.fetchFromGitHub {
      owner = "fifi2";
      repo = "gtd.vim";
      rev = "e0201b7696087e5a534560a539d95c9f618fa491";
      sha256 = "1y03fdirg5xgz16lic686zjf6xap0phy53qfn4hp0xmlgd00di5a";
    };
  };

  browser = [
    "brave-browser.desktop"
  ];
  associations = {
    "application/pdf" = [ "zathura" ];
    "text/html" = browser;
    "x-scheme-handler/http" = browser;
    "x-scheme-handler/https" = browser;
    "x-scheme-handler/ftp" = browser;
    "x-scheme-handler/chrome" = browser;
    "x-scheme-handler/about" = browser;
    "x-scheme-handler/unknown" = browser;
    "application/x-extension-htm" = browser;
    "application/x-extension-html" = browser;
    "application/x-extension-shtml" = browser;
    "application/xhtml+xml" = browser;
    "application/x-extension-xhtml" = browser;
    "application/x-extension-xht" = browser;
    "application/x-ms-dos-executable" = [ "wine.desktop" ];
    "application/epub+zip" = [ "calibre-ebook-viewer.desktop" ];
  };
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = userInfo.username;
  home.homeDirectory = userInfo.homeDirectory;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";

  home.packages = myPackages;

  #services.emacs.package = pkgs.emacsUnstable;

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.autojump = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.htop = {
    enable = true;
  };

  programs.rofi = {
    enable = true;
    theme = "gruvbox-dark-hard";
  };

  #programs.mbsync = {
  #  enable = true;
  #  extraConfig = builtins.readFile ./.mbsyncrc;
  #};

  programs.emacs = {
    enable = true;
    #package = pkgs.emacsGcc;
    package = pkgs.emacs;
  };

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile ./tmux.conf;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "web-search"
      ];
      theme = "theunraveler";
    };
    shellAliases = {
      vi = "nvim";
      py = "python3";
      rdm = "vi README.md";
      ls = "exa";
      sl = "ls --color=auto";
      lls = "ls --color=auto";
      lss = "ls --color=auto";
      cwd = "pwd";
      pw = "poweroff";
      viconf = "vi ~/.vimrc";
      zshconf = "vi ~/.zshrc";
      down = "git pull";
      cat = "bat";
    };

    initExtra = builtins.readFile ./.zshrc;
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
    withPython3 = true;
    plugins = with pkgs.vimPlugins; [
      { plugin = vimtex;
        config = ''
          let g:tex_flavor='latex'
          let g:vimtex_view_method='zathura'
          let g:vimtex_quickfix_mode=0
          set conceallevel=1
          let g:tex_conceal='abdmg'
        '';
      }
      { plugin = vimtex;
        config = ''
          let g:UltiSnipsExpandTrigger = '<tab>'
          let g:UltiSnipsJumpForwardTrigger = '<tab>'
          let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
        '';
      }
      vim-snippets
      vim-nix
      { plugin = vimwiki;
        config = ''
          let g:vimwiki_list = [{'path': '~/vimwiki/', 'syntax': 'markdown', 'ext': '.md'}]
        '';
      }
      vim-zettel
      fzf-vim
      { plugin = vim-which-key;
        config = ''
          let g:mapleader = "\<Space>"
          nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
        '';
      }
      vim-startify
      { plugin = vim-gtd;
        config = ''
          let g:gtd#dir = "~/gtd"
	  let g:gtd#review = [
		\ '(!inbox + !scheduled-'.strftime("%Y%m%d").') @work',
		\ '!todo @work',
		\ '!waiting @work',
		\ ]
        '';
      }
    ];
  };

  programs.nushell = {
    enable = true;
  };

  xdg.enable = true;
  xdg.configFile = {
    "nu/keybindings.yml" = {
      text = ''
        # https://github.com/nushell/nushell/blob/main/docs/sample_config/keybindings.yml
        # Complete Hint
        - key:
            Ctrl: S
          binding:
            CompleteHint:
      '';
    };
  };

  programs.git = {
    enable = true;
    userName = userInfo.gitUsername;
    userEmail = userInfo.gitEmail;
    extraConfig = {
      alias = {
        co = "checkout";
        br = "branch";
        ci = "commit";
        st = "status";
      };
    };
  };

  home.file."secrets".text = builtins.readFile ./secrets;

  xdg.configFile."termonad/termonad.hs".text = builtins.readFile ./termonad.hs;

  systemd.user.services.neuron = let
    notesDir = "${userInfo.homeDirectory}/zettelkasten";
    # See "Declarative Install"
    neuron = (
      let neuronRev = "44855fb8674e74a6b9a6688a8dff0298e9c78124";
          neuronSrc = builtins.fetchTarball "https://github.com/srid/neuron/archive/${neuronRev}.tar.gz";
       in import neuronSrc {});
  in {
    Unit.Description = "Neuron zettelkasten service";
    Install.WantedBy = [ "default.target" ];
    Service = {
      ExecStart = "${neuron}/bin/neuron -d ${notesDir} rib -ws 127.0.0.1:8081";
    };
  };

  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;

      config = pkgs.writeText "xmonad.hs" (builtins.readFile ./xmonad.hs);
    };
  };

  xdg.configFile."xmobar/.xmobarrc".text = builtins.readFile ./.xmobarrc;

  home.file = {
    ".config/xmobar" = {
      source = ./xpm;
      recursive = true;
    };
  };

  services.dunst = {
    enable = true;

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
      size = "48x48";
    };

    settings = {
      global = {
        font = "mononoki Nerd Font Mono 14";
        allow_markup = "yes";
        icon_position = "left";
        max_icon_size = 32;
        sort = "yes";
        indicate_hidden = "yes";
        alignment = "left";
        show_age_threshold = 30;
        word_wrap = "yes";
        ignore_newline = "no";
        geometry = "300x5+30-50";
        transparency = 20;
        idle_threshold = 120;
        monitor = 0;
        follow = "mouse";
        sticky_history = "yes";
        line_height = 0;
        separator_height = 2;
        padding = 8;
        horizontal_padding = 8;
        separator_color = "frame";
        startup_notification = "false";
        frame_width = 3;
      };
      shortcuts = {
        close = "ctrl+space";
        close_all = "ctrl+shift+space";
        history = "ctrl+dollar";
        context = "ctrl+shift+period";
      };
      urgency_low = {
        background = "#282828";
        foreground = "#ebdbb2";
        frame_color = "#83a598";
        timeout = 10;
      };
      urgency_normal = {
        background = "#282828";
        foreground = "#ebdbb2";
        frame_color = "#83a598";
        timeout = 10;
      };
      urgency_critical = {
        background = "#282828";
        foreground = "#ebdbb2";
        frame_color = "#83a598";
        timeout = 0;
      };
    };
  };

  xdg.mime.enable = true;

  xdg.mimeApps = {
    enable = true;
    defaultApplications = associations;
    associations.added = associations;
  };

  targets.genericLinux.enable = true;
}
