#!/bin/bash

asdf plugin-add direnv
asdf direnv setup --shell zsh --version latest
asdf global direnv latest

asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf install nodejs lts
asdf global nodejs lts

asdf plugin-add python
asdf install python 3.11.1
asdf global python 3.11.1
asdf reshim python

asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf install ruby 3.2.0
asdf global ruby 3.2.0

asdf plugin-add golang https://github.com/kennyp/asdf-golang.git
asdf install golang 1.19.5
asdf global golang 1.19.5
asdf reshim golang

asdf plugin add tfenv
asdf install tfenv latest
asdf global tfenv latest

asdf plugin-add terraform https://github.com/asdf-community/asdf-hashicorp.git
asdf install terraform latest
asdf global terraform latest
