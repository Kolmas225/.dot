for bin_path in ~/.local/bin ~/.bin
    if test -d bin_path
        if not contains -- bin_path $PATH
            set -p PATH bin_path
        end
    end
end

if status is-interactive
    # Commands to run in interactive sessions can go here

    function fish_greeting
    end

    abbr -a e exit

    if [ (command -v emacs) ]
        abbr -a ecc emacsclient -nc
        abbr -a ect emacsclient -t
        source ~/.config/fish/conf.d/emacs-vterm.fish
    end

    if [ (command -v eza) ]
        alias ls='eza --icons --group-directories-first'
        alias la='ls -a'
        alias ll='ls -lb'
        alias lla='ll -a'
    end

    if [ (command -v helix) ]
        abbr -a hl helix
    else if [ (command -v hx) ]
        abbr -a hl hx
    end

    function gitconf
        git config user.name $argv[1]
        git config user.email $argv[2]
    end

    abbr -a ghgit gitconf Kolmas225 "169401425+Kolmas225@users.noreply.github.com"
    abbr -a cbgit gitconf Kolmas "kolmas@noreply.codeberg.org"

    # Go to git root directory
    abbr -a cdgr 'cd (git rev-parse --show-toplevel)'

    # TODO now works in fish 4.0
    function fish_should_add_to_history
        # for cmd in vault mysql ls
        for cmd in rm
            string match -qr "^$cmd" -- $argv; and return 1
        end
        return 0
    end

    function stow-init
        cd ~/.dot/
        stow bin bash
        stow --no-folding term helix emacs misc media
        cd -
    end
    function restow-all
        cd ~/.dot/
        stow -R bin bash
        stow -R --no-folding term helix emacs misc media
        cd -
    end

    function rq
        ruby -rjson -e 'ip = JSON.parse(ARGF.read);'"$argv" ;
    end
end

# zoxide
zoxide init fish --cmd cd | source

# rbenv installed via pacman
rbenv init - | source

# with install script
# ~/.rbenv/bin/rbenv init - --no-rehash fish | source
