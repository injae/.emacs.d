export LANG=ko_KR.UTF-8 # emacs libvterm korea encoding fix

#source "$HOME/.zinit/zinit.zsh"
source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

zicompinit
# Load a few important annexes, without Turbo
# (this is currently required for annexes)
ZSH_THEME="lambda"

setopt promptsubst

zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

zinit snippet OMZP::git
zinit snippet OMZP::pip
zinit snippet OMZP::command-not-found
zinit snippet OMZP::colored-man-pages

zinit ice as"completion"
#zinit snippet OMZP::docker/_docker

zinit ice blockf atpull'zinit creinstall -q .'

autoload compinit
compinit

zinit light djui/alias-tips
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma-continuum/fast-syntax-highlighting
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-history-substring-search
zinit light MichaelAquilina/zsh-you-should-use
zinit light iam4x/zsh-iterm-touchbar


### End of Zinit's installer chunk

eval "$(starship init zsh)"


export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
#eval "$(pyenv virtualenv-init -)"
#eval "$(pipenv --completion)"
#alias python='python3'
#alias pip='pip3'

# neovim setting
#export EDITOR=/usr/local/bin/nvim
#alias vim='lvim'
#alias vi='lvim'
#alias vimdiff="lvim -d"
#alias nvim='lvim'

# direnv
eval "$(direnv hook zsh)"

# emacs setting
#export EDITOR=emacsclient
export EDITOR=nvim
export VISUAL=$EDITOR

# emacs vterm setting
export TERM=xterm-256color    
unsetopt prompt_cr prompt_sp
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

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

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";

}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
#
#vterm_cmd() {
#    local vterm_elisp
#    vterm_elisp=""
#    while [ $# -gt 0 ]; do
#        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
#        shift
#    done
#    vterm_printf "51;E$vterm_elisp"
#}
#========

# zsh-iterm-touchbar setting
TOUCHBAR_GIT_ENABLED=true
YARN_ENABLED=true

# Ruby setting
# macos home-brew rbenv setting
# rvm setting
# macos don't use apple clang
case "$OSTYPE" in
darwin*)
    # ...
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
    alias clang=clang-14
    alias clang++=clang++-14
    alias gcc=gcc-12
    alias g++=g++-12
    export NVM_DIR="$HOME/.nvm"
    [ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
    [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
;;
linux*)
    alias clang=clang-14
    alias clang++=clang++-14
    export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew";
    export HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar";
    export HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew";
    export PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin${PATH+:$PATH}";
    export MANPATH="/home/linuxbrew/.linuxbrew/share/man${MANPATH+:$MANPATH}:";
    export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH:-}";
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
;;
dragonfly*|freebsd*|netbsd*|openbsd*)
    # ...
;;
esac

export GO111MODULE=on
# windows wsl setting
if [[ $(uname -r) =~ WSL ]]; then
    export GOROOT=$HOME/dev/tool/go
    export GOPATH=$HOME/go
    export PATH=$GOROOT/bin:$GOPATH/bin:$PATH
fi

export CC=clang
export CXX=clang++
export AR=llvm-ar
export RANLIB=llvm-ranlib
alias CC=$CC
alias CXX=$CXX
alias cc=$CC

#alias ld=$LD
#alias ar=$AR
#alias ranlib=$RANLIB
# more fast system command , use rust base command
alias ls='exa -g --time-style=long-iso'
alias cat='bat'
alias find='fd'
alias grep='rg'
alias ps='procs'
alias top='ytop'
#alias python='rustpython'

# kuberneties setting
alias kub=kubectl
#source "$(kubectl completion zsh)"

alias vmrun='/Applications/VMWare\ Fusion.app/Contents/Library/vmrun'
alias vmsee='vmrun list'
alias vmstart='vmrun start ~/Virtual\ Machines.localized/Windows\ 10\ x64.vmwarevm nogui'
alias vmstop='vmrun suspend ~/Virtual\ Machines.localized/Windows\ 10\ x64.vmwarevm'

export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}

[[ -s "/home/nieel/.gvm/scripts/gvm" ]] && source "/home/nieel/.gvm/scripts/gvm"

