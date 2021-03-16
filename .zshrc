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
