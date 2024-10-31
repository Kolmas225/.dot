#!/usr/bin/env bash

set -e

echo "Building & Installing Emacs"

EMACS_SOURCE_DIR="$HOME/src/emacs"
EMACS_BRANCH="emacs-30"
EMACS_GIT_SOURCE="https://github.com/emacs-mirror/emacs.git"

if [ -d "$EMACS_SOURCE_DIR" ]; then
    read -p "DIR already exist, proceed with current emacs source? (y/N)" yn

    case $yn in
        yes | y) ;;
        no | n | "")
            echo "Exiting..."
            exit
            ;;
        *)
            echo "Invalid response, exiting..."
            exit 1
            ;;
    esac

else
    git clone -b $EMACS_BRANCH $EMACS_GIT_SOURCE "$EMACS_SOURCE_DIR"
fi

cd "$EMACS_SOURCE_DIR"

read -p "Ready to build emacs, proceed? (y/N)" yn
case $yn in
    yes | y) ;;
    no | n | "")
        echo "Exiting..."
        exit
        ;;
    *)
        echo "Invalid response, exiting..."
        exit 1
        ;;
esac

./autogen.sh
./configure \
    --with-pgtk --with-x-toolkit=no \
    --with-native-compilation=aot \
    --with-tree-sitter \
    --without-gpm \
    CFLAGS='-O2 -march=native'

make "-j$(nproc)"
sudo make install

echo "Finished Building & Installing Emacs"
