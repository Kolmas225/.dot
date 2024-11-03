#!/usr/bin/env fish

# will move to DIR as stay
set SCRIPT_DIR (cd (dirname (status -f)); and pwd) 

# Install fisher
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher

# theme
fisher install catppuccin/fish
fish_config theme save "Catppuccin Macchiato"

# pure prompt
fisher install pure-fish/pure

# setup rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# setup rbenv
curl -fsSL https://rbenv.org/install.sh | bash

if test -f $XDG_CONFIG_HOME/fish/config.fish 
    rm $XDG_CONFIG_HOME/fish/config.fish
end
stow --no-folding fish
