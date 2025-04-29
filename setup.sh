#!/usr/bin/env bash

set -e

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

baseSetupPacman() {
    sudo pacman -Syyu base-devel stow git curl wget unzip fd ripgrep \
         unarchiver fish helix ghostty zoxide yazi eza \
         tree-sitter libgccjit
}

baseSetupDnf() {
    sudo dnf install stow git wget curl \
         fd ripgrep unar fish helix zoxide eza \
         tree-sitter libgccjit
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

mkdir -p "$HOME/src"

bash ./emacs-treesit-module.sh

fish ./setup-fish.fish

