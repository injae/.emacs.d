export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/.pyenv/bin:$PATH"
export PATH="$HOME/.pyenv/shims:$PATH"
export PATH="$HOME/.cppm/bin:$PATH"
export PATH="$HOME/.ghcup/bin:$PATH"
export PATH="/usr/local/opt/ruby/bin:$PATH"
export PATH="$PATH:$HOME/.rvm/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/vcpkg:$PATH"
export PATH="$PATH:$HOME/go/bin"
export PATH="$PATH:/Users/nieel/.local/bin"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$HOME/.cppm/local/lib/pkg-config:/usr/local/opt/libpq/lib/pkgconfig"
# kotline language server
export PATH="$HOME/dev/tools/kotlin-language-server:$PATH"
# openjdk setting
export PATH="/usr/local/opt/openjdk/bin:$PATH"
# telepresence kubectl 정보수집 비활성화
export SCOUT_DISABLE=1

## llvm@9 setting
#export PATH="/usr/local/opt/llvm@9/bin:$PATH"
#export LDFLAGS="-L/usr/local/opt/llvm@9/lib"
#export CPPFLAGS="-I/usr/local/opt/llvm@9/include"

# llvm@stable setting
export PATH="/usr/local/opt/llvm/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/llvm/lib"

# aseprite setting
export ASEPRITE_USER_FOLDER="$HOME/.config/aseprite/"

# rust sdl2 link setting

export SDKROOT="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"

#export KUBECONFIG="$HOME/.config/kubernetes/admin.conf"
export KUBECONFIG="$HOME/.kube/config"
# helm local docker registry flag
export HELM_EXPERIMENTAL_OCI=1

export TF_PLUGIN_CACHE_DIR="$HOME/.terraform.d/plugin-cache"

# terraform version manager option
# git clone https://github.com/tfutils/tfenv.git ~/.tfenv
export PATH="$HOME/.tfenv/bin:$PATH"

# emacs lsp-mode 
export LSP_USE_PLISTS=true

#if [ -e /home/nieel/.nix-profile/etc/profile.d/nix.sh ]; then . /home/nieel/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
