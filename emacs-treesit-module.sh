#!/usr/bin/env bash

echo "Installing tree-sitter-module for emacs"

FILE_NAME="libs-linux-x64.zip"
TMP_DIR="/tmp"

SRC_DIR="$HOME/src"
if ! [ -d $SRC_DIR ] ; then
    mkdir $SRC_DIR
fi

ZIP_FILE=$(curl -s "https://api.github.com/repos/casouri/tree-sitter-module/releases/latest" |
    rg browser_download.*$FILE_NAME |
    cut -d : -f 2,3 |
    tr -d \")

TAG=$(echo $ZIP_FILE |
    cut -d / -f 8)

DEST_DIR="$SRC_DIR/tree-sitter-module-$TAG"

if [ -d $DEST_DIR ] ; then
    echo "Folder already exist, exitting..."
else
    echo "Downloading..."
    if [ -f $TMP_DIR/$FILE_NAME ] ; then
        rm $TMP_DIR/$FILE_NAME
    fi
    curl -Lo $TMP_DIR/$FILE_NAME $ZIP_FILE
    mkdir $DEST_DIR
    unzip -d $DEST_DIR -j $TMP_DIR/$FILE_NAME
    rm $TMP_DIR/$FILE_NAME
    echo "Finished installing tree-sitter-module"
fi

