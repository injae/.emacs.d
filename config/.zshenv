#!/bin/zsh
# telepresence kubectl 정보수집 비활성화
export SCOUT_DISABLE=1

# aseprite setting
export ASEPRITE_USER_FOLDER="$HOME/.config/aseprite/"

# rust sdl2 link setting
# export SDKROOT="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"

#export KUBECONFIG="$HOME/.config/kubernetes/admin.conf"
export KUBECONFIG="$HOME/.kube/config"
# helm local docker registry flag
export HELM_EXPERIMENTAL_OCI=1

export TF_PLUGIN_CACHE_DIR="$HOME/.terraform.d/plugin-cache"

# emacs lsp-mode 
export LSP_USE_PLISTS=true

# enchant setting
export ENCHANT_CONFIG_DIR="$HOME/.config/enchant"

# rust sccache setting
export RUSTC_WRAPPER=sccache

export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export PATH="$PATH:$HOME/.local/share/rtx/shims"
