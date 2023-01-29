#!/bin/bash

# yay installed package
# asdf-vm 0.11.1-2
# emacs-git 30.0.50.164738-1
# ttf-d2coding 1.3.2-2
# ttf-nanum 2018-1
# yay-bin 11.3.2-1



cp ./config/.zshrc ~/.zshrc
cp ./config/.zshenv ~/.zshenv
cp ./config/.gitconfig ~/.gitconfig

# zinit install
bash -c "$(curl --fail --show-error --silent --location https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"

# /etc/locale.gen에서 주석 제거
sudo locale-gen ko_KR.UTF-8

asdf plugin-add direnv
asdf direnv setup --shell zsh --version latest
asdf global direnv latest

asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf install nodejs lts
asdf global nodejs lts

asdf plugin-add python
asdf global python 3.11.1

asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf install ruby 3.2.0
asdf global ruby 3.2.0
