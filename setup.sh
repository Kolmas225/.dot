#!/usr/bin/env bash

set -e

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

baseSetupPacman() {
    sudo pacman -Syyu base-devel stow git curl wget unzip fd ripgrep \
         unarchiver fish helix wezterm zoxide yazi \
         tree-sitter libgccjit
}

baseSetupDnf() {
    sudo dnf install git wget curl fd ripgrep unar fish helix zoxide \
         tree-sitter libgccjit

    # Fedora's repo don't have wezterm
    wget -o /tmp/wezterm.rpm https://github.com/wez/wezterm/releases/download/nightly/wezterm-nightly-fedora40.rpm
    sudo dnf install /tmp/wezterm.rpm
    rm /tmp/wezterm.rpm
}

# baseSetupApt () {
#     #
# }

# Determining pacakge manager
PACK=("pacman" "dnf" "apt")
CURRENT_PACK=false

for p in "${PACK[@]}"; do
    if command -v "$p" >/dev/null; then
        CURRENT_PACK="$p"
    fi
done

case $CURRENT_PACK in
"pacman")
    baseSetupPacman
    ;;

"dnf")
    baseSetupDnf
    ;;

"apt")
    echo "Setup not available yet"
    ;;

*)
    echo "Setup not available"
    ;;
esac

cd "$SCRIPT_DIR"

rm -rf "$HOME/.bashrc" "$HOME/.bash_profile" "$HOME/.bashrc.d"
stow bash bin
stow --no-folding term helix emacs misc media

bash ./emacs-treesit-module.sh

fish ./setup-fish.fish

