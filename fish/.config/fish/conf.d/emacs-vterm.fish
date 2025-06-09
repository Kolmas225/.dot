# from https://github.com/pymander/vfish/blob/main/conf.d/vfish.fish

if [ "$INSIDE_EMACS" = 'vterm' ]
    function vterm_printf
        if begin
                [ -n "$TMUX" ]; and string match -q -r "screen|tmux" "$TERM"
            end
            # tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
        else if string match -q -- "screen*" "$TERM"
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$argv"
        else
            printf "\e]%s\e\\" "$argv"
        end
    end

    # Completely clear the buffer. With this, everything that is not on screen
    # is erased.
    function clear
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    end

    # This is to change the title of the buffer based on information provided by the
    # shell. See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the meaning of the
    # various symbols.
    function fish_title
        hostname
        echo ":"
        prompt_pwd
    end

    # With vterm_cmd you can execute Emacs commands directly from the shell.
    # For example, vterm_cmd message "HI" will print "HI".
    # To enable new commands, you have to customize Emacs's variable
    # vterm-eval-cmds.
    function vterm_cmd --description 'Run an Emacs command among the ones defined in vterm-eval-cmds.'
        set -l vterm_elisp ()
        for arg in $argv
            set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
        end
        vterm_printf '51;E'(string join '' $vterm_elisp)
    end

    function vf --description 'Open a file for editing in Emacs from vterm'
        vterm_cmd find-file (realpath "$argv")
    end

    function vd --description 'Run dired on a directory from vterm'
        set -f dir "$argv"
        if test x = "x$dir"
            set dir (pwd)
        end
        vterm_cmd dired (realpath "$dir")
    end

    function vdiff --argument-names filea fileb --description 'Run ediff-files on files A and B'
        vterm_cmd ediff-files (realpath "$filea") (realpath "$fileb")
    end
end
