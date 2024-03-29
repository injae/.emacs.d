#!/bin/bash

asdf plugin-add direnv
asdf direnv setup --shell zsh --version latest
asdf global direnv latest

asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf install nodejs lts
asdf global nodejs lts

asdf plugin-add python
asdf install python latest
asdf global python latest
asdf reshim python

asdf plugin add ruby https://github.com/asdf-vm/asdf-ruby.git
asdf install ruby 3.2.0
asdf global ruby 3.2.0

asdf plugin-add golang https://github.com/kennyp/asdf-golang.git
asdf install golang latest
asdf global golang latest
asdf reshim golang

asdf plugin add tfenv
asdf install tfenv latest
asdf global tfenv latest

asdf plugin-add terraform https://github.com/asdf-community/asdf-hashicorp.git
asdf install terraform latest
asdf global terraform latest

asdf plugin-add bazel https://github.com/mrinalwadhwa/asdf-bazel.git
asdf install bazel 6.2.1
asdf global bazel 6.2.1
