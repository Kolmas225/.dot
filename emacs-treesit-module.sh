#!/usr/bin/env bash

set -e

echo "Installing tree-sitter-module for emacs"

SRC_DIR="$HOME/src"

if [ -d "$SRC_DIR/tree-sitter-module" ]
then
    cd "$SRC_DIR/tree-sitter-module"
    git pull --force
else
    git clone "https://github.com/casouri/tree-sitter-module/" "$SRC_DIR/tree-sitter-module"
    cd "$SRC_DIR/tree-sitter-module"
fi

JOBS=$(nproc) ./batch.sh

rm -rf "$XDG_CONFIG_HOME/emacs/tree-sitter"
mv ./dist "$XDG_CONFIG_HOME/emacs/tree-sitter"

echo "Finished installing tree-sitter-module"
