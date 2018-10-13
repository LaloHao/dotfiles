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
antigen bundle dijitalmunky/nvm-auto

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
source /usr/share/nvm/init-nvm.sh
