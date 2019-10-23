# Created by newuser for 5.7.1


source $HOME/.antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle docker
antigen bundle pip
antigen bundle command-not-found
antigen bundle djui/alias-tips
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle MichaelAquilina/zsh-you-should-use

antigen theme lambda

antigen apply

#eval "$(starship init zsh)"

function chpwd() {
    print -Pn "\e]51;A$(pwd)\e\\";
}

export PATH="/home/nieel/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export PATH=$PATH:/home/nieel/.cppm/local/bin

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='printf "\e]51;E(vterm-clear-scrollback)\e\\";tput clear'
fi
