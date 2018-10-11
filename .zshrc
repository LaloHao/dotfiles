export EDITOR=et
export ZSH=/usr/share/oh-my-zsh
source $ZSH/oh-my-zsh.sh

source /usr/share/zsh/share/antigen.zsh
antigen bundle git-prompt
antigen bundle command-not-found
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle zsh-users/zsh-autosuggestions

PROMPT='
%{$fg_bold[yellow]%}%~/ %{$reset_color%} $(git_super_status)
%{$fg_bold[blue]%}λ %{$reset_color%}'

antigen apply

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line

# function chpwd() {
#     emulate -L zsh
#     ls -a
# }

eval $(thefuck --alias)

# function mkdir {
#     command mkdir $1 && cd $1
# }

alias dl="youtube-dl -x --audio-format mp3"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm use
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc
