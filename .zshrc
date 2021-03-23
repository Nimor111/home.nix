# motd
pfetch

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

# 256-color terminal
export TERM=xterm-256color

# direnv setup hook
eval "$(direnv hook zsh)"

# nix
. $HOME/.nix-profile/etc/profile.d/nix.sh

# Secrets
source secrets

eval "$(jump shell)"

export NODE_OPTIONS=--max-old-space-size=2048

export EDITOR=nvim

export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale

export GUIX_PROFILE=$HOME/.config/guix/current
. $GUIX_PROFILE/etc/profile

export PATH=$PATH:$HOME/.local/bin

# Vterm settings
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
