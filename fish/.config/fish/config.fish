if status is-interactive
    # Commands to run in interactive sessions can go here

    abbr -a e exit
    abbr -a ecc emacsclient -nc
    abbr -a ect emacsclient -t
    
    if [ (command -v eza) ]
        alias ls='eza --icons --group-directories-first'
        alias la='ls -a'
        alias ll='ls -lb'
        alias lla='ll -a'
    end

    # if [ (command -v helix) ]
    #     set -gx EDITOR helix
    #     abbr -a hl helix
    # else
    #     set -gx EDITOR hx
    #     abbr -a hl hx
    # end

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

    zoxide init fish --cmd cd | source
end

status --is-interactive; and ~/.rbenv/bin/rbenv init - --no-rehash fish | source
