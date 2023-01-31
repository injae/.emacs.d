#!/bin/bash
cp .zshrc ~/.zshrc
cp .zshenv ~/.zshenv
cp .gitconfig ~/.gitconfig

# zinit install
bash -c "$(curl --fail --show-error --silent --location https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"

# /etc/locale.gen에서 주석 제거
sudo locale-gen ko_KR.UTF-8

# clone private config
git clone https://github.com/injae/private_config.git private
