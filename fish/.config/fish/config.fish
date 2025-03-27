if status is-interactive
    # Commands to run in interactive sessions can go here

    # Variables
    set -gx NOTES_DIR "~/Documents/org/denote"

    # abbr for the deck
    set current_os (cat /etc/os-release | rg "^ID=\w+\$" | cut -d = -f 2)
    if [ current_os = "bazzite " ]
        abbr -a dbe distrobox enter -n bazzite-arch-gnome
        abbr -a dbef distrobox enter -n bazzite-arch-gnome -- fish -i
    end
    set -e current_os

    abbr -a e exit
    # abbr -a n $EDITOR $NOTES_DIR
    abbr -a gu gitui
    abbr -a ecc emacsclient -nc
    abbr -a ect emacsclient -t
    abbr -a imgcat wezterm imgcat

    if [ (command -v helix) ]
        set -gx EDITOR helix
        abbr -a hl helix
    else
        set -gx EDITOR hx
        abbr -a hl hx
    end

    # yazi
    function y
        set tmp (mktemp -t "yazi-cwd.XXXXXX")
        yazi $argv --cwd-file="$tmp"
        if set cwd (command cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
            builtin cd -- "$cwd"
        end
        rm -f -- "$tmp"
    end

    function gitconf
        git config user.name $argv[1]
        git config user.email $argv[2]
    end

    abbr -a ghgit gitconf Kolmas225 "169401425+Kolmas225@users.noreply.github.com"
    abbr -a cbgit gitconf Kolmas "kolmas@noreply.codeberg.org"

    # TODO now works in fish 4.0
    function fish_should_add_to_history
        # for cmd in vault mysql ls
        for cmd in rm
            string match -qr "^$cmd" -- $argv; and return 1
        end
        return 0
    end

    zoxide init fish --cmd cd | source
end

status --is-interactive; and ~/.rbenv/bin/rbenv init - --no-rehash fish | source
