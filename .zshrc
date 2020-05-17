eval "$(direnv hook zsh)"

export EDITOR=et
export ZSH=/Users/lalo/.oh-my-zsh
source $ZSH/oh-my-zsh.sh

source /Users/lalo/dev/antigen.zsh
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

