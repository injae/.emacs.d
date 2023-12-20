#!/bin/zsh

export LANG="en_US.UTF-8" 
export LANGUAGE="ko_KR.UTF-8"

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

zicompinit

autoload -Uz compinit
compinit

autoload -U bashcompinit
bashcompinit

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
zinit snippet https://github.com/docker/cli/blob/master/contrib/completion/zsh/_docker

zinit light zsh-users/zsh-completions
zinit light djui/alias-tips
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma-continuum/fast-syntax-highlighting


zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-history-substring-search
zinit light MichaelAquilina/zsh-you-should-use
zinit light iam4x/zsh-iterm-touchbar
zinit load agkozak/zsh-z

### End of Zinit's installer chunk

eval "$(starship init zsh)"

# rtx setting
eval "$(rtx activate zsh)"

# emacs setting
export EDITOR=emacsclient
alias edit=emacsclient
#export EDITOR=nvim
#export VISUAL=$EDITOR

# emacs vterm setting
#export TERM=xterm-256color    
#unsetopt prompt_cr prompt_sp

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

# zsh-iterm-touchbar setting
TOUCHBAR_GIT_ENABLED=true
YARN_ENABLED=true

case "$OSTYPE" in
darwin*)
    # c/cpp compiler
    alias clang=clang-17
    alias clang++=clang++-17
    alias gcc=gcc-13
    alias g++=g++-13

    # vm controller
    alias vmrun="/Applications/VMWare\ Fusion.app/Contents/Library/vmrun"
    alias vmsee="vmrun list"
    alias vmstart="vmrun start ~/Virtual\ Machines.localized/Windows\ 10\ x64.vmwarevm nogui"
    alias vmstop="vmrun suspend ~/Virtual\ Machines.localized/Windows\ 10\ x64.vmwarevm"

    # ruby open-ssl fix
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"

    # llvm stable
    export PATH="/usr/local/opt/llvm/bin:$PATH"
    export LDFLAGS="-L/usr/local/opt/llvm/lib"

    export PATH="/usr/local/opt/openjdk/bin:$PATH"
    export PATH="/usr/local/sbin:$PATH"

    export PATH=~/.local/bin:$PATH
;;
linux*)
    # alias clang=clang-13
    # alias clang++=clang++-13

    # wsl vpn-kit start command
    alias vpn-start="wsl.exe -d wsl-vpnkit --cd /app service wsl-vpnkit start"
;;
dragonfly*|freebsd*|netbsd*|openbsd*)
    # ...
;;
esac

export GO111MODULE=on

export CC=clang
export CXX=clang++
export AR=llvm-ar
export RANLIB=llvm-ranlib

alias CC=$CC
alias CXX=$CXX
alias cc=$CC

alias ld=$LD
alias ar=$AR
alias ranlib=$RANLIB

# more fast system command , use rust base command
if [ -x "$(command -v exa)" ]; then
    alias ls='exa -g --time-style=long-iso'
fi
if [ -x "$(command -v bat)" ]; then
    alias cat='bat'
fi
if [ -x "$(command -v fd)" ]; then
    alias find='fd'
fi
if [ -x "$(command -v rg)" ]; then
    alias grep='rg'
fi
if [ -x "$(command -v procs)" ]; then
    alias ps='procs'
fi
if [ -x "$(command -v ytop)" ]; then
    alias top='ytop'
fi
if [ -x "$(command -v et)" ]; then
    alias tree='et -I'
fi
if [ -x "$(command -v mcfly)" ]; then
    eval "$(mcfly init zsh)"
fi

alias ppytest='poetry run pytest'
alias pr='poetry run python'

# kuberneties setting
# source "$(kubectl completion zsh)"

export PATH="$PATH:$HOME/.ghcup/bin"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:$HOME/vcpkg"
export PATH="$PATH:$HOME/.local/bin"

export PATH="$PATH:$HOME/.cppm/bin"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$HOME/.cppm/local/lib/pkg-config:/usr/local/opt/libpq/lib/pkgconfig"
# openjdk setting


vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

eval "$(register-python-argcomplete pipx)"
