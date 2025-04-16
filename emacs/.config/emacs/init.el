;;; init.el ---  -*- lexical-binding: t -*-

;; (setq debug-on-error t)
;; (debug-on-entry 'load-file)
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq use-package-always-ensure t
      use-package-enable-imenu-support t)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode))

;; Block until current queue processed.
(elpaca-wait)

(keymap-global-set "C-c e" #'elpaca-log)
(keymap-global-set "C-c E" #'elpaca-update-all)

(defvar my/autosave-dir (concat user-cache-directory "auto-saves/"))

;; Functions
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun my/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

;; MAYBE use consult maybe
(defun my/emacs-config ()
  "Emacs config."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun my/project-root-or-current-directory ()
  "Return project root or current directory."
  (or (and (project-current)
           (project-root (project-current)))
      default-directory))

(defun my/open-line-and-indent (n)
  "Like `newline-and-indent' for the `open-line' command."
  (interactive "*p")
  (let ((inhibit-message t)
        (eol (copy-marker (line-end-position))))
    (open-line n)
    (indent-region (point) eol)
    (set-marker eol nil)))

;; taken from doom
(defun my/empty-newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (beginning-of-line)
  (save-excursion (newline))
  (indent-according-to-mode))

(defun my/empty-newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(keymap-global-set "C-S-<return>" #'my/empty-newline-above)
(keymap-global-set "C-<return>" #'my/empty-newline-below)

;; Keymaps

(keymap-set mode-specific-map "f p" #'my/emacs-config)
(keymap-set mode-specific-map "f d" #'my/delete-file-and-buffer)

(keymap-set mode-specific-map "q Q" #'save-buffers-kill-emacs)
(keymap-set mode-specific-map "q q" #'save-buffers-kill-terminal)
(keymap-set mode-specific-map "q r" #'restart-emacs)

;; NOTE enable some disabled commands
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page
               dired-find-alternate-file
               scroll-left))

;; disable commands
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(overwrite-mode))

;;; TODO: auth-source
(use-package auth-source
  :ensure nil
  :custom
  (auth-sources '("~/.authinfo.gpg")))

;;; epa
(use-package epa
  :ensure nil)

;; built-in
(use-package emacs
  :ensure nil
  :custom
  (tab-width 4)
  (c-basic-offset 4)
  (c-ts-mode-indent-offset 4)
  (help-at-pt-display-when-idle t)
  (help-at-pt-timer-delay 0.3)
  (echo-keystrokes 0.01)
  (truncate-lines t)
  (initial-scratch-message nil)
  (enable-recursive-minibuffers t)
  (find-file-visit-truename t)
  (tab-always-indent 'complete)
  ;; skip command that won't work on the current mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  (kill-do-not-save-duplicates t)
  (mode-require-final-newline 'visit-save)
  (delete-pair-blink-delay 0.1)   ;don't really use delete-pair tho
  (global-auto-revert-mode t)
  ;; disable the annoying backup files
  (make-backup-files nil)
  (create-lockfiles nil)
  (auto-save-file-name-transforms `((".*" ,my/autosave-dir t)))
  (auto-save-list-file-prefix (concat my/autosave-dir "saves-"))
  (backup-directory-alist `((".*" . ,(concat user-cache-directory "backup/"))))
  ;; decrease the mark limits to be more efficient
  (mark-ring-max 8)
  (global-mark-ring-max 10)
  (kill-ring-max 50)
  (history-delete-duplicates t)
  (save-interprogram-paste-before-kill t)
  ;; scrolling
  (scroll-preserve-screen-position t)
  (scroll-conservatively 10)
  ;; tab-bar
  (tab-bar-show t)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-auto-width nil)
  ;; prefer spaces to tabs
  (indent-tabs-mode nil)
  ;; browse-url
  (browse-url-browser-function #'browse-url-firefox)
  (browse-url-firefox-program "firefox-flatpak-private")
  ;; emacs 30+: disable ispell completion function in favour of `cape-dict'
  (text-mode-ispell-word-completion nil)
  :hook
  (after-init . column-number-mode)
  (after-init . minibuffer-depth-indicate-mode)
  (after-init . undelete-frame-mode)
  :bind
  (("C-h K" . #'describe-keymap)
   ("C-h C-h" . nil)
   ("C-o" . #'my/open-line-and-indent)
   ("C-M-g" . #'keyboard-quit)
   ("C-M-o" . #'backward-up-list)
   ("C-M-u" . #'up-list)
   ("C-S-k" . #'my/backward-kill-line)
   ("C-S-l" . #'my/horizontal-recenter)
   ("C-S-o" . #'open-line)
   ("M-z" . nil)                        ;default: zap-to-char
   ("M-=" . #'count-words)
   ("<f2>" . nil)           ;default: 2C-mode
   ("<f8>" . #'repeat)
   ("<f10>" . nil)  ;default: context menu
   ;; indentation
   ("S-<left>" . #'indent-rigidly-left-to-tab-stop)
   ("S-<right>" . #'indent-rigidly-right-to-tab-stop)
   ;; casing
   ("M-c" . #'capitalize-dwim)
   ("M-l" . #'downcase-dwim)
   ("M-u" . #'upcase-dwim)
   ;; scroll
   ("C-<next>" . #'my/scroll-right-half-window)
   ("C-<prior>" . #'my/scroll-left-half-window)
   (:map mode-specific-map
         ("o x" . #'scratch-buffer)
         ("t v" . #'visual-line-mode))
   (:map ctl-x-map
         ("k" . #'kill-current-buffer)
         ("M-t" . #'transpose-sentences))
   (:map ctl-x-r-map
         ("M-d" . #'my/clear-all-registers)))
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; NOTE: taken from https://stackoverflow.com/a/1249665
  (defun my/horizontal-recenter ()
    "make the point horizontally centered in the window"
    (interactive)
    (let ((mid (/ (window-width) 2))
          (line-len (save-excursion (end-of-line) (current-column)))
          (cur (current-column)))
      (if (< mid cur)
          (set-window-hscroll (selected-window)
                              (- cur mid)))))
  
  (defun my/scroll-right-half-window ()
    (interactive)
    (scroll-right (/ (window-width) 2)))
  (defun my/scroll-left-half-window ()
    (interactive)
    (scroll-left (/ (window-width) 2)))

  (defun my/backward-kill-line (&optional arg)
    (interactive "P")
    (if arg
        (kill-line (- arg))
      (kill-line 0)))

  (defun my/clear-all-registers ()
    "Clear all registers"
    (interactive)
    (setq register-alist nil)
    (message "All registers are cleared!")))

;; bookmark
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file (concat user-data-directory "bookmarks"))
  (bookmark-fringe-mark nil)   ;emacs 29
  (bookmark-save-flag 1)       ;save to file for every bookmark change
  )

;; compile-mode
(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error t)
  :bind
  ("<f5>" . #'recompile)
  ("C-<f5>" . #'compile)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  ;; ref: https://github.com/LionyxML/emacs-solo/blob/df3354b01ef9128a70d7213805e6d5ef63209d4e/init.el#L240C3-L242C80
  (defun my/ignore-compilation-status (&rest _)
    (setq compilation-in-progress nil))
  (advice-add 'compilation-start :after #'my/ignore-compilation-status))

(use-package compile-multi
  :custom
  (compile-multi-default-directory #'my/project-root-or-current-directory)
  :bind
  ("M-<f5>" . #'compile-multi))
(use-package consult-compile-multi
  :after compile-multi
  :config (consult-compile-multi-mode))
(use-package compile-multi-nerd-icons
  :after (compile-multi nerd-icons-completion))
(use-package compile-multi-embark
  :after (compile-multi embark)
  :config (compile-multi-embark-mode))

(use-package delsel
  :ensure nil
  :init
  (delete-selection-mode 1))

(use-package hl-line
  :ensure nil
  :init
  (global-hl-line-mode 1))

(use-package info
  :ensure nil
  :init
  (add-to-list 'Info-default-directory-list "~/src/info/"))

(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file (concat user-cache-directory "recentf"))
  :init
  (recentf-mode 1)
  :config
  (dolist (var '("\\.gpg\\'" "\\.asc\\'" "\\.sig\\'" "\\.ssh/"
                 "/Private/" "^/tmp/"
                 "/COMMIT_EDITMSG\\'"
                 "/\\.git/"))
    (add-to-list 'recentf-exclude var)))

(use-package repeat
  :ensure nil
  :custom
  (repeat-exit-timeout 5)
  (repeat-exit-key "<escape>")
  (repeat-echo-function #'repeat-echo-message)
  (set-mark-command-repeat-pop t)
  :init
  (repeat-mode 1))

;; TODO: History persistance
(use-package savehist
  :ensure nil
  :custom
  (savehist-file (concat user-cache-directory "savehist-history"))
  :init
  (savehist-mode 1))

;; Emacs server (allow emacsclient to connect to running session)
(use-package server
  :ensure nil
  :custom
  (server-client-instructions nil)
  :config
  (unless (server-running-p)
    (server-start)))

;;; Tramp
(use-package tramp
  :ensure nil
  :init
  (setq tramp-persistency-file-name (concat user-cache-directory "tramp")))

(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style
   '(face tabs spaces trailing
          lines-tail
          space-mark tab-mark
          missing-newline-at-eof
          space-before-tab
          space-after-tab))
  :bind
  ("C-c t SPC" . #'whitespace-mode))

(use-package winner
  :ensure nil
  :custom
  (winner-dont-bind-my-keys t)
  :hook (after-init . winner-mode)
  :bind
  ("<Back>" . #'winner-undo)
  ("<Forward>" . #'winner-redo))

;;; Utils
(use-package helpful
  :bind
  ("C-h C-k" . #'helpful-kill-buffers)
  ("C-h F" . #'helpful-function)
  ([remap describe-function] . #'helpful-callable)
  ([remap describe-variable] . #'helpful-variable)
  ([remap describe-command] . #'helpful-command)
  ([remap describe-key] . #'helpful-key)
  ([remap describe-symbol] . #'helpful-symbol)
  (:map emacs-lisp-mode-map
        ("C-h C-." . #'helpful-at-point)))

(use-package elisp-demos
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;; Font

(set-face-attribute 'default nil :font "Maple Mono-14")
(set-face-attribute 'fixed-pitch nil :font "Maple Mono-14")
(set-face-attribute 'fixed-pitch-serif nil :font "Maple Mono-14")

(set-face-attribute 'variable-pitch nil :family "Inter-14")

;; CJK font
(dolist (script '(cjk-misc han kana hangul))
  (set-fontset-font t script "LXGW WenKai Screen"))

(use-package ligature
  :config
  ;; JetBrains Mono
  ;; disabled "-<"
  ;; removed from org-mode: "***"
  (let ((jetbrains-base-ligatures
         '("--" "---" "==" "===" "!=" "!==" "=!="
           "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" ";;" "!!"
           "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
           "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
           "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
           "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
           "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
           "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<<" ">->" "<-<" "<-|"
           "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
           "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
           "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
           ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
           "<:<" ";;;")))
    
    (ligature-set-ligatures
     'prog-mode
     (append jetbrains-base-ligatures '("***")))

    (ligature-set-ligatures
     'org-mode
     (append jetbrains-base-ligatures)))
  
  (global-ligature-mode t))

;;; UI

(use-package emacs
  :ensure nil
  :config
  ;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-hook 'window-setup-hook #'toggle-frame-fullscreen))

;; (use-package doom-themes
;;   :custom
;;   (doom-themes-enable-bold t)
;;   (doom-themes-enable-italic t)
;;   :config
;;   (load-theme 'doom-moonlight :no-confirm)
;;   ;; (load-theme 'doom-challenger-deep :no-confirm)
;;   ;; (set-face-attribute 'secondary-selection nil :background (doom-color 'base5))
;;   ;; (set-face-attribute 'Info-quoted nil :foreground (doom-color 'magenta))

;;   ;; Enable flashing mode-line on errors
;;   ;; (doom-themes-visual-bell-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package modus-themes
;;   :custom
;;   (modus-themes-italic-constructs t)
;;   (modus-themes-bold-constructs t)
;;   :config
;;   (setq modus-themes-common-palette-overrides
;;  '((fringe unspecified)
;;    (fg-line-number-inactive "gray50")
;;           (fg-line-number-active fg-main)
;;           (bg-line-number-inactive unspecified)
;;           (bg-line-number-active unspecified)))
;;   (load-theme 'modus-vivendi :no-confirm))

(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'macchiato)
  (catppuccin-highlight-matches t)
  :config
  (load-theme 'catppuccin :no-confirm)
  (set-face-attribute 'secondary-selection nil :background (catppuccin-color 'overlay0)))

(use-package doom-modeline
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-total-line-number t)
  (doom-modeline-battery t)
  (doom-modeline-workspace-name nil)
  :config
  (doom-modeline-mode))

(use-package paren
  :ensure nil
  :custom
  (show-paren-context-when-offscreen 'overlay))

(use-package which-func
  :ensure nil
  :custom
  (which-func-modes '(prog-mode))
  ;; (which-func-display 'header)
  (which-func-format `((:propertize (" ➤ " which-func-current)
                                    local-map ,which-func-keymap
                                    face which-func
                                    mouse-face mode-line-highlight
                                    help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")))
  :hook
  (after-init . which-function-mode)
  :config
  (setq which-func-unknown "N/A"))

(use-package rainbow-delimiters
  ;; :custom
  ;; (rainbow-delimiters-max-face-count 4)
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :custom
  (global-highlight-parentheses-mode t)
  (highlight-parentheses-delay 0.2)
  (highlight-parentheses-colors 'nil)
  (highlight-parentheses-attributes
   '((:underline t))))

(use-package colorful-mode
  :custom
  (colorful-use-prefix nil)
  :bind
  ("C-c t c" . #'colorful-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode))

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start t)
  (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . #'display-line-numbers-mode))

(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t))

(use-package hl-todo
  ;; HACK https://github.com/alphapapa/magit-todos/issues/171#issuecomment-1934362142
  ;; :ensure (:depth nil)
  :ensure (:branch "main")
  :bind
  ("C-c i h" . #'hl-todo-insert)
  :config
  (global-hl-todo-mode))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; (use-package transpose-frame)

;; nerd-icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font")
  :bind
  ("C-x 8 n" . #'nerd-icons-insert))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; (use-package visual-fill-column
;;   :bind
;;   ("C-c t v" . visual-fill-column-mode)
;;   :init
;;   ;; (global-visual-fill-column-mode 1)
;;   (setq-default visual-fill-column-center-text t)
;;   (setq visual-fill-column-fringes-outside-margins t))

;;; Completion

(use-package vertico
  :custom
  ;; (resize-mini-windows t)        ;resizing as results narrow down
  (vertico-resize nil)
  (vertico-count 13)
  (vertico-cycle t)
  :bind
  (:map vertico-map
        ("DEL" . #'vertico-directory-delete-char)
        ("C-<backspace>" . #'vertico-directory-delete-word)
        ("M-DEL" . #'vertico-directory-delete-word)
        ("M-s" . #'vertico-toggle-sort))
  :init
  (vertico-mode 1)
  :config  
  ;; toggle between history-length-alpha & alpha only
  (defun vertico-toggle-sort ()
    (interactive)
    (setq-local vertico-sort-override-function
                (and (not vertico-sort-override-function)
                     #'vertico-sort-alpha)
                vertico--input t)))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :custom
  (vertico-multiform-commands
   '((org-insert-link flat)))
  (vertico-multiform-categories
   '((embark-keybinding grid)))
  :init
  (vertico-multiform-mode 1))

(use-package vertico-quick
  :ensure nil
  :after vertico
  :custom
  (vertico-quick1 "neiaho")
  (vertico-quick2 "crstdy")
  ;; unsetting the face then inherit from avy
  :custom-face
  (vertico-quick1 ((t (:foreground unspecified :background unspecified :inherit avy-lead-face))))
  (vertico-quick2 ((t (:foreground unspecified :background unspecified :inherit avy-lead-face-0))))
  :bind
  (:map vertico-map
        ("C-." . #'vertico-quick-exit)))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((eglot (styles . (orderless basic)))))
  (orderless-component-separator #'orderless-escapable-split-on-space) ; Use backslash for literal space
  )

(use-package consult
  :custom
  (consult-narrow-key "<")
  (consult-preview-excluded-files
   '("\\`/[^/|:]+:"                     ;remote file
     "\\.gpg\\'" "\\.asc\\'" "\\.sig\\'" "\\.ssh/"
     "Private/"))
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :bind
  ("C-c M-x" . #'consult-mode-command)
  ;; replacing some vanilla commands
  ([remap yank-pop] . #'consult-yank-pop)
  ([remap goto-line] . #'consult-goto-line)
  ([remap imenu] . #'consult-imenu)
  ("C-'" . #'consult-imenu)
  ("C-M-'" . #'consult-imenu-multi)
  ([remap repeat-complex-command] . #'consult-complex-command)
  ;; consult-buffer
  ([remap switch-to-buffer] . #'consult-buffer)
  ([remap switch-to-buffer-other-window] . #'consult-buffer-other-window)
  ([remap switch-to-buffer-other-tab] . #'consult-buffer-other-tab)
  ([remap switch-to-buffer-other-frame] . #'consult-buffer-other-frame)
  ([remap list-buffers] . #'consult-buffer)
  ([remap project-switch-to-buffer] . #'consult-project-buffer)
  ;; registers
  ("C-M-#" . #'consult-register)
  ("M-'" . #'consult-register-store)
  ("M-#" . #'consult-register-load)
  ([remap bookmark-jump] . #'consult-bookmark)
  (:map search-map
        ("M-s" . #'consult-line)
        ("s" . #'consult-line-multi)
        ("M-f" . #'consult-fd)
        ("M-r" . #'consult-ripgrep)
        ("M-e" . #'consult-isearch-history)
        ("M-i" . #'consult-info)
        ("M-l" . #'consult-locate))
  (:map minibuffer-local-map
        ;; ("M-s" . #'consult-history) ;; orig. next-matching-history-element
        ("M-r" . #'consult-history)) ;; orig. previous-matching-history-element
  (:map goto-map
        ("M-f" . #'consult-flymake)
        ("M-e" . #'consult-compile-error)
        ("M-j" . #'consult-mark)
        ("j" . #'consult-global-mark)
        ("M-o" . #'consult-outline)
        ("M-r" . #'consult-recent-file))
  :init
  
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.3
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  
  :config

  ;; live previews
  (consult-customize consult-ripgrep :preview-key "M-."
                     consult-theme :preview-key '(:debounce 0.5 any))
  
  ;; Use Orderless as pattern compiler for consult-grep/ripgrep/find
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler))

(use-package consult-dir
  :custom
  (consult-dir-jump-file-command 'consult-fd) ;use consult-fd
  (consult-dir-sort-candidates t)   ;enable candidate sorting
  :bind
  ("C-x C-d" . consult-dir)
  (:map vertico-map
        ("C-x C-d" . #'consult-dir)
        ("C-x C-j" . #'consult-dir-jump-file)))

;; BUG: have some bugs
(use-package consult-todo
  :bind
  (:map search-map
        ("t" . #'consult-todo)
        ("M-t" . #'consult-todo-all)))

(use-package embark
  :custom
  (embark-indicators
   '(embark-minimal-indicator  ; default is embark-mixed-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  ;; (embark-prompter #'embark-completing-read-prompter) ;always prompt for action
  :bind
  ("C-;" . #'embark-act)
  ("C-M-;" . #'embark-dwim)
  ([remap describe-bindings] . #'embark-bindings)
  (:map embark-file-map
        ("0" . #'make-empty-file))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-preselect 'directory)
  (corfu-popupinfo-max-height 20)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay 0.2)
  ;; (corfu-echo-delay 0.3)
  (corfu-on-exact-match 'show)
  :bind
  (:map corfu-map
        ("RET" . nil)
        ("C-/" . #'corfu-reset))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  ;; (corfu-echo-mode)
  :config

  ;; add more command that will trigger corfu-auto
  (dolist (com '("\\`backward-kill-word"
                 "\\`kill-word"
                 "delete-forward-char\\'"))
    (add-to-list 'corfu-auto-commands com))
  
  ;; Completing in the minibuffer for some commands
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package corfu-quick
  :ensure nil
  :after corfu
  :custom
  (corfu-quick1 "neiaho")
  (corfu-quick2 "crstdy")
  :custom-face
  (corfu-quick1 ((t (:foreground unspecified :background unspecified :inherit avy-lead-face))))
  (corfu-quick2 ((t (:foreground unspecified :background unspecified :inherit avy-lead-face-0))))
  :bind
  (:map corfu-map
        ("C-." . #'corfu-quick-complete)))

(use-package cape
  :custom
  (cape-dabbrev-min-length 5)
  :bind
  ("C-c /" . cape-prefix-map)
  :init
  
  (defun my/elisp-setup-capf ()
    (setq-local completion-at-point-functions
                '(cape-elisp-symbol t)))
  (add-hook 'emacs-lisp-mode-hook #'my/elisp-setup-capf)

  (defun my/elisp-block-capf ()
    (setq-local completion-at-point-functions
                `(cape-elisp-block
                  ,@completion-at-point-functions)))
  (add-hook 'org-mode-hook #'my/elisp-block-capf)
  (add-hook 'markdown-mode-hook #'my/elisp-block-capf)

  ;; eglot
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (dolist (capfs '(cape-keyword
                   cape-file
                   cape-dict
                   cape-dabbrev))
    (add-to-list 'completion-at-point-functions capfs t)))

;;; Eldoc
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil)
  :config
  ;; from doom
  (defun my/emacs-lisp-append-value-to-eldoc-a (fn sym)
    "Display variable value next to documentation in eldoc."
    (when-let (ret (funcall fn sym))
      (if (boundp sym)
          (concat ret " "
                  (let* ((truncated " [...]")
                         (print-escape-newlines t)
                         (str (symbol-value sym))
                         (str (prin1-to-string str))
                         (limit (- (frame-width) (length ret) (length truncated) 1)))
                    (format (format "%%0.%ds%%s" (max limit 0))
                            (propertize str 'face 'warning)
                            (if (< (length str) limit) "" truncated))))
        ret)))
  (advice-add #'elisp-get-var-docstring
              :around
              #'my/emacs-lisp-append-value-to-eldoc-a))

(use-package eldoc-box
  :custom
  (eldoc-box-clear-with-C-g t)
  :bind
  (:map eglot-mode-map
        ("C-M-<prior>" . #'my/eldoc-box-scroll-up)
        ("C-M-<next>" . #'my/eldoc-box-scroll-down)
        ("C-h ." . #'eldoc-box-help-at-point))
  :init
  (defun my/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 5))))
  (defun my/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 5)))))

;;; Templates
(use-package tempel
  :custom
  ;; (tempel-trigger-prefix "&")
  (tempel-path (concat user-data-directory "templates/*.eld"))
  :bind
  ("M-+" . #'tempel-expand)
  ("C-c i t" . #'tempel-insert)
  :config
  ;; NOTE corfu-on-exact-match is 'show globally
  ;; but 'insert works better with tempel-expand
  (when (featurep 'corfu)
    (advice-add #'tempel-expand :around
                (lambda (f &rest args)
                  "call `tempel-expand' with
`corfu-on-exact-match' being 'insert"
                  (let ((corfu-on-exact-match 'insert))
                    (apply f args))))))

;; TODO copy only needed templates instead of pulling the whole
;; package
(use-package tempel-collection
  :after tempel)

;; abbrev
(use-package abbrev
  :ensure nil
  :custom
  (abbrev-file-name (concat user-data-directory "abbrev")))

;;; Editing

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package puni
  :hook
  ((prog-mode sgml-mode nxml-mode tex-mode
              lisp-data-mode
              org-mode
              eval-expression-minibuffer-setup) . puni-mode)
  :bind
  (:map puni-mode-map
        ("M-(" . nil)
        ("M-)" . nil)
        ("C-{" . #'puni-syntactic-backward-punct)
        ("C-}" . #'puni-syntactic-forward-punct)
        ;; ("C-(" . #'puni-contract-region)
        ;; ("C-)" . #'puni-expand-region)
        ("C-<backspace>" . #'puni-backward-kill-word)
        ("C-M-t" . #'puni-transpose)
        ("C-, m" . #'puni-mark-sexp-around-point)
        ("C-, /" . #'puni-split)
        ("C-, ." . #'puni-raise)
        ("C-, ," . #'puni-splice)
        ("C-, i" . #'puni-splice-killing-backward)
        ("C-, e" . #'puni-splice-killing-forward)
        ("C-, n" . #'puni-slurp-backward)
        ("C-, a" . #'puni-slurp-forward)
        ("C-, N" . #'puni-barf-backward)
        ("C-, A" . #'puni-barf-forward)
        ("C-, c" . #'puni-convolute)
        ("C-, k" . #'puni-squeeze))
  :config
  ;; TEMP find a better way to do this
  (add-hook 'corfu-mode-hook
            (lambda ()
              (dolist (com '("\\`puni-backward-kill-word"
                             "\\`puni-backward-delete-char"
                             "puni-forward-kill-word\\'"
                             "puni-forward-delete-char\\'"))
                (add-to-list 'corfu-auto-commands com)))))

(use-package expreg
  :bind
  ("C-)" . expreg-expand)
  ("C-(" . expreg-contract))

(use-package grep
  :ensure nil
  :custom
  (grep-use-headings t))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package electric
  :ensure nil
  :config
  (electric-pair-mode 1))

(use-package vundo
  :bind ("C-x u" . #'vundo))

;;; Navigation

(use-package avy
  :custom
  (avy-background t)
  (avy-single-candidate-jump nil)
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-keys '(?n ?e ?i ?a ?t ?s ?r ?c))
  (avy-dispatch-alist
   '((?x . avy-action-kill-move)
     (?d . avy-action-kill-stay)
     (?p . avy-action-teleport)
     (?m . avy-action-mark)
     (?w . avy-action-easy-kill)
     (?v . avy-action-copy)
     (?y . avy-action-yank)
     (?z . avy-action-zap-to-char)
     (?. . avy-action-embark)
     (?T . avy-action-exchange)))
  :custom-face
  (avy-background-face ((nil (:foreground "gray40" :background unspecified))))
  :bind
  ("C-." . #'avy-goto-word-or-subword-1)
  :config
  ;; Taken from karthink's avy blog
  (defun avy-action-embark (pt)
    "embark-act on PT."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  
  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))

  ;; yoink'd from karthink's config
  ;; [2024-07-18]
  ;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el#L45
  (defun avy-action-easy-kill (pt)
    "Basically doing `easy-kill' action on target."
    (unless (require 'easy-kill nil t)
      (user-error "Easy Kill not found, please install."))
    (cl-letf* ((bounds (if (use-region-p)
                           (prog1 (cons (region-beginning) (region-end))
                             (deactivate-mark))
                         (bounds-of-thing-at-point 'sexp)))
               (transpose-map
                (define-keymap
                  "M-t" (lambda () (interactive "*")
                          (pcase-let ((`(,beg . ,end) (easy-kill--bounds)))
                            (transpose-regions (car bounds) (cdr bounds) beg end
                                               'leave-markers)))))
               ((symbol-function 'easy-kill-activate-keymap)
                (lambda ()
                  (let ((map (easy-kill-map)))
                    (set-transient-map
                     (make-composed-keymap transpose-map map)
                     (lambda ()
                       ;; Prevent any error from activating the keymap forever.
                       (condition-case err
                           (or (and (not (easy-kill-exit-p this-command))
                                    (or (eq this-command
                                            (lookup-key map (this-single-command-keys)))
                                        (let ((cmd (key-binding
                                                    (this-single-command-keys) nil t)))
                                          (command-remapping cmd nil (list map)))))
                               (ignore
                                (easy-kill-destroy-candidate)
                                (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
                                  (easy-kill-save-candidate))))
                         (error (message "%s:%s" this-command (error-message-string err))
                                nil)))
                     (lambda ()
                       (let ((dat (ring-ref avy-ring 0)))
                         (select-frame-set-input-focus
                          (window-frame (cdr dat)))
                         (select-window (cdr dat))
                         (goto-char (car dat)))))))))
      (goto-char pt)
      (easy-kill))))

(use-package link-hint
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))


(use-package ace-window
  :ensure
  (:host github :repo "Kolmas225/ace-window" :branch "fork" :files ("*.el"))
  :custom
  (aw-dispatch-always t)
  (aw-scope 'frame)
  (aw-keys '(?n ?e ?i ?a ?t ?s ?r ?c))
  (aw-leading-char-style 'path)
  (ace-window-display-mode t)	     ;display dispatch char at modeline
  :custom-face
  (aw-leading-char-face ((nil (:foreground unspecified :inherit avy-lead-face))))
  :bind
  ("M-o" . #'ace-window)
  ("M-O" . #'ace-window-prefix)
  :config
  
  (advice-add #'aw--switch-buffer :override
	          (lambda () (consult-buffer)))

  (defun ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  
  (defun my/aw-kill-buffer (window)
    "Select a window with ace-window and kill the buffer."
    (with-selected-window window
      (kill-buffer)))
  
  (defun my/aw-kill-buffer-and-window (window)
    "Select a window with ace-window and kill the buffer and window."
    (with-selected-window window
      (kill-buffer-and-window)))
  
  (setq aw-dispatch-alist
	    '((?m aw-swap-window "Swap Window")
	      (?M aw-move-window "Move Window")
	      (?y aw-copy-window "Yank Window")
	      (?b aw-switch-buffer-in-window "Select Buffer")
	      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	      (?d aw-delete-window "Delete Window")
	      (?\; aw-execute-command-other-window "Execute Command Other Window")
	      ;; (?S aw-split-window-fair "Split Sensible Window")
	      (?h aw-split-window-vert "Split Vert Window")
	      (?v aw-split-window-horz "Split Horz Window")
	      (?o delete-other-windows "Delete Other Windows")
          (?k my/aw-kill-buffer "Kill Current Buffer")
          (?K my/aw-kill-buffer-and-window "Kill Buffer and Window")
	      (?T aw-transpose-frame "Transpose Frame")
	      (?\M-o aw-flip-window)	;M-o M-o
	      (?? aw-show-dispatch-help "Show help"))))

(use-package transpose-frame
  :bind
  (:map ctl-x-4-map
        ("t" . #'transpose-frame)))

;;; easy-kill
(use-package easy-kill
  :bind
  ([remap kill-ring-save] . #'easy-kill)
  ([remap mark-sexp] . #'easy-mark)
  (:map easy-kill-base-map
        ("C-w" . #'easy-kill-region)
        ("C-g" . #'easy-kill-abort)
        ;; ("C-M-@" . #'easy-kill-)
        ("x" . #'easy-kill-exchange-point-and-mark))
  :config
  ;; HACK: stop echoing current candidate in minibuffer
  (advice-add 'easy-kill-describe-candidate :override #'ignore)
  ;; move cursor to the beginning when marking
  (advice-add 'easy-mark
              :after
              (lambda (&rest _)
                (call-interactively
                 #'easy-kill-exchange-point-and-mark))))

;;; isearch
(use-package isearch-mb
  :custom
  (isearch-lazy-count t)
  (isearch-regexp-lax-whitespace t)
  (search-whitespace-regexp ".*?")
  (search-ring-max 50)
  (regexp-search-ring-max 50)
  (lazy-count-prefix-format "[%s/%s] ")
  (isearch-repeat-on-direction-change t)
  (isearch-wrap-pause nil)      ;not needed with `isearch-mb' keybinds
  :bind
  (:map isearch-mb-minibuffer-map
        ("M-e"  . #'consult-isearch-history)
        ("C-w" . #'isearch-yank-word-or-char)
        ("C-M-w" . #'isearch-yank-symbol-or-char)
        ("C-." . #'avy-isearch)
        ("M-s M-s" . #'consult-line))
  :config
  (isearch-mb-mode 1)
  (add-to-list 'isearch-mb--with-buffer #'consult-isearch-history)
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word-or-char)
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-symbol-or-char)
  (add-to-list 'isearch-mb--after-exit #'avy-isearch)
  (add-to-list 'isearch-mb--after-exit #'consult-line))

;;; symbol-overlay
(use-package symbol-overlay
  :bind
  ("M-n" . #'symbol-overlay-jump-next)
  ("M-p" . #'symbol-overlay-jump-prev))

;;; eshell
(use-package eshell
  :ensure nil
  :custom
  (eshell-directory-name (expand-file-name "eshell/" user-cache-directory))
  (eshell-banner-message "\n")
  :bind
  ("C-c o e" . #'eshell)
  :config
  ;; taken from stackoverflow: https://emacs.stackexchange.com/a/81154
  (defun my/eshell-add-aliases (name definition)
    "Define eshell alias NAME with DEFINITION unless it's defined."
    (unless (eshell-lookup-alias name)
      (eshell/alias name definition)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              "Initialize custom eshell aliases."
              (my/eshell-add-aliases "ff" "find-file $1")
              (my/eshell-add-aliases "vf" "view-file $1")
              (my/eshell-add-aliases "e" "exit")
              (my/eshell-add-aliases "clear" "eshell/clear"))))

(use-package eshell-prompt-extras
  :after eshell
  :config
  (setq eshell-highlight-prompt t
        eshell-prompt-regexp "^[^λ]+ λ "
        eshell-prompt-function 'epe-theme-multiline-with-status))

;; (use-package esh-autosuggest
;;   :hook (eshell-mode . esh-autosuggest-mode))

(use-package fish-completion
  :after eshell
  :config
  (when (executable-find "fish")
    (global-fish-completion-mode)))

(use-package esh-help
  :after (eshell fish-completion)
  :config
  (setup-esh-help-eldoc))

;; ;; TODO: 
;; (use-package eat
;;   :hook
;;   (eshell-load . eat-eshell-mode)
;;   (eshell-load . eat-eshell-visual-command-mode))

;;; Dired
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches
   "-AGFhlv --group-directories-first --time-style=long-iso")
  (dired-free-space nil)
  (dired-kill-when-opening-new-dired-buffer t)
  ;; dired-aux
  (dired-do-revert-buffer t)
  :hook
  (dired-mode . dired-isearch-filenames-mode)
  :bind
  ("C-x d" . #'dired-jump)
  (:map dired-mode-map
        ("C-+" . dired-create-empty-file)))

(use-package wdired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("C-c C-e" . #'wdired-change-to-wdired-mode)))

(use-package popper
  :custom
  (popper-window-height (floor (frame-height) 3))
  ;; (popper-group-function #'popper-group-by-directory)
  :bind
  ("C-`" . popper-toggle)
  ("M-`" . popper-cycle)
  ("C-~" . popper-toggle-type)
  ("C-M-~" . popper-kill-latest-popup)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*scratch\\*"
          "^\\*.*eshell.*\\*$" eshell-mode ;eshell as a popup
          "\\*ielm\\*"
          help-mode
          helpful-mode
          compilation-mode
          ;; org-mode
          "^\\*Org Select"
          "^\\*Org Links"
          "^\\*Org Agenda"
          "^\\*Capture\\*$\\|CAPTURE-.*$"
          ;; rust
          "\\*cargo-.*\\*"
          "\\*rustic-.*\\*"
          ))
  (popper-mode 1)
  (popper-echo-mode 1))

;;; Magit
(use-package magit
  :ensure
  (:branch "main" :pre-build ("make" "info"))
  :custom
  ;; (magit-delete-by-moving-to-trash nil)
  (magit-copy-revision-abbreviated t)
  ;; TODO: clean this up
  (magit-status-sections-hook
   '(magit-insert-status-headers
     magit-insert-merge-log
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     magit-insert-bisect-output
     magit-insert-bisect-rest
     magit-insert-bisect-log
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-stashes
     magit-insert-unpushed-to-pushremote
     magit-insert-unpushed-to-upstream
     magit-insert-unpulled-from-pushremote
     magit-insert-unpulled-from-upstream
     magit-insert-recent-commits))
  :bind
  ("C-x g" . #'magit-status))

;;; transient
(use-package transient
  :ensure (:branch "main")
  :custom
  (transient-history-file (concat user-cache-directory "transient-history.el"))
  (transient-values-file (concat user-cache-directory "transient-values.el"))
  (transient-levels-file (concat user-cache-directory "transient-levels.el"))
  :bind
  (:map transient-map
        ("<escape>" . transient-quit-one)))

;;; Spellchecking
(use-package jinx
  :custom
  (jinx-languages "en_GB")
  :bind
  ("M-$" . #'jinx-correct)
  ("C-M-$" . #'jinx-languages))

;;; Org

(use-package org
  :ensure nil
  :custom
  (org-directory "~/Documents/org/")
  (org-id-locations-file (concat user-cache-directory "org-id-locations"))  
  (org-startup-folded "show2levels")
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-refile-targets '((nil :maxlevel . 3)))
  (org-archive-location "~/Documents/org/archive.org::* From %s")
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-pretty-entities t)
  :bind
  ("C-c l s" . #'org-store-link)
  ("C-M-<return>" . #'org-insert-subheading)
  (:map org-mode-map
        ("C-'" . #'consult-org-heading)
        ("C-," . nil)))

;; TODO: Finish up capture templates
(use-package org-capture
  :ensure nil
  :after (org denote)
  :custom
  (org-default-notes-file (expand-file-name "capture.org" org-directory))
  ;; prevent creation of bookmarks from org-capture
  (org-capture-bookmark nil)
  (org-bookmark-names-plist nil)
  (org-capture-templates
   '(("t" "Todo" entry
      (file+headline "todo.org" "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
     ("n" "Notes" entry
      (file+headline "notes.org" "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
     ("j" "Journal" entry
      (file+olp+datetree "journal.org")
      "* %U %?\n%i\n%a" :prepend t)
     ("d" "Denote" plain
      (file denote-last-path)
      #'denote-org-capture)
     ;; TODO add more templates
     ))
  :bind
  ("C-c c" . #'org-capture)
  :config
  (org-capture-put :kill-buffer t
                   :no-save t))

(use-package org-agenda
  :ensure nil
  :after org
  :custom
  (org-agenda-files (list org-directory))
  (org-agenda-tags-column 0)
  :bind
  (:map mode-specific-map
        ("a a" . #'org-agenda))
  :config
  (setq org-todo-keywords
        ;; Taken from doom
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
          )))

;; org-babel & source blocks
(use-package org
  :ensure nil
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  ;; trust the programmer, it can never go wrong
  (org-confirm-babel-evaluate nil)
  :config
  (setf (alist-get :noweb org-babel-default-header-args) "strip-export")
  (add-to-list 'org-babel-tangle-lang-exts '("scheme" . "scm"))
  (add-to-list 'org-src-lang-modes '("sh" . bash-ts))
  (add-to-list 'org-src-lang-modes '("bash" . bash-ts))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (python . t)
     (ruby . t))))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package org-modern
  :after org
  :custom
  (org-modern-star 'replace)
  (org-modern-hide-stars nil)
  ;; (org-modern-table nil)
  (org-modern-list
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  (org-modern-block-name '("" . "")) ; or other chars; so top bracket is drawn promptly
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(use-package org-modern-indent
  :ensure
  (org-modern-indent
   :host github
   :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
  ;; HACK https://github.com/jdtsmith/org-modern-indent/issues/10#issuecomment-1671726529
  (add-hook 'org-mode-hook (lambda () (aset org-indent--text-line-prefixes 0 (propertize " " 'face 'org-indent)))))

;; for codeblock highlighting in exporting to html
(use-package htmlize)

;; scrolling through inline image in org-mode
(use-package org-sliced-images
  :custom
  (org-sliced-images-round-image-height 1)
  :config
  (org-sliced-images-mode 1))

;;; Denote
(use-package denote
  :custom
  (denote-directory (expand-file-name "~/Documents/org/denote/"))
  (denote-file-name-components-order '(title signature keywords identifier))
  (denote-prompts '(title keywords subdirectory))
  (denote-rename-confirmations nil)
  (denote-history-completion-in-prompts nil)
  (denote-known-keywords
   '("ongoing" "backlog" "done"
     "meta"
     "test"))
  ;; (denote-templates
  ;;  )
  (denote-save-buffers t)
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  (denote-rename-buffer-mode t)
  :hook (dired-mode . denote-dired-mode)
  :bind
  ("C-c n n" . #'denote-open-or-create)
  ("C-c n N" . #'denote-open-or-create-with-command)
  ("C-c n i" . #'denote-link-or-create)
  ("C-c n I" . #'denote-add-links)
  ("C-c n l" . #'denote-find-link)
  ("C-c n b" . #'denote-find-backlink)
  ("C-c n B" . #'denote-backlinks)
  ("C-c n r" . #'denote-rename-file)
  ;; dired sort
  ("C-c n d" . #'denote-sort-dired)
  ;; keywords
  ("C-c n w" . #'denote-rename-file-keywords)
  ;; template
  ("C-c n t" . #'denote-template)
  (:map dired-mode-map
        ("r r" . #'denote-dired-rename-files)
        ("r w" . #'denote-dired-rename-marked-files-with-keywords))
  :config
  ;; default denote-subdirectory-prompt to denote-directory and
  ;; dismissing completion history
  (defun my-denote--subdirs-completion-table (dirs)
    "Match DIRS as a completion table."
    (let* ((table (denote--completion-table 'file dirs))
           (prompt (format "Select SUBDIRECTORY [%s]: " denote-directory)))
      (completing-read prompt table nil t nil nil denote-directory)))
  (advice-add #'denote--subdirs-completion-table
              :override #'my-denote--subdirs-completion-table))

(use-package denote-org
  :after denote
  :ensure
  (:host github :repo "protesilaos/denote-org")
  :bind
  ("C-c n h" . #'denote-org-link-to-heading)
  ("C-c n H" . #'denote-org-backlinks-for-heading)
  ;; extract org-subtree to a new denote
  ("C-c n e" . #'denote-org-extract-org-subtree)
  ;; org dynamic blocks
  ("C-c n o l" . #'denote-org-dblock-insert-links)
  ("C-c n o b" . #'denote-org-dblock-insert-backlinks)
  ("C-c n o f" . #'denote-org-dblock-insert-files))

(use-package denote-silo
  :after denote
  :ensure
  (:host github :repo "protesilaos/denote-silo")
  :custom
  (denote-silo-directories
   `(,(denote-directory)
     ,(expand-file-name "dumps/" "~/Private/")
     ,(expand-file-name "consume/" "~/Private/")
     ;; ,(expand-file-name "temp/" "~/src")
     ))
  :bind
  ("C-c n x" . #'denote-silo-open-or-create)
  ("C-c n X" . #'denote-silo-select-silo-then-command))

(use-package consult-denote
  :after (consult denote)
  :ensure
  (:host github :repo "protesilaos/consult-denote")
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  (consult-denote-find-command #'consult-fd)
  (consult-denote-mode t)
  :bind
  ("C-c n s" . #'consult-denote-grep)
  ("C-c n S" . #'consult-denote-grep-in-silo)
  :config
  
  (consult-customize consult-denote-grep consult-denote-find
                     :preview-key "M-.")
  
  (defun consult-denote-grep-in-silo ()
    "Call `consult-denote-grep-command' in a silo."
    (declare (interactive-only t))
    (interactive)
    (let ((denote-directory (denote-silo-directory-prompt)))
      (funcall-interactively consult-denote-grep-command (denote-directory)))))

;;; Media
;; TODO: problabaly can make a transient for easier controls
(use-package empv
  :custom
  (empv-audio-file-extensions '("mp3" "ogg" "wav" "m4a" "flac" "aac" "ape"))
  (empv-video-file-extensions '("mkv" "mp4" "avi" "mov" "webm"))
  (empv-base-directory (expand-file-name "~/Music/"))
  (empv-invidious-instance "https://invidious.perennialte.ch/api/v1")
  (empv-radio-channels
   '(("RTHK-1 (news)" . "https://rthk.hk/live1.m3u")
     ("RTHK-2 (culture)" . "https://rthk.hk/live2.m3u")
     ("RTHK-3 (english)" . "https://rthk.hk/live3.m3u")
     ("RTHK-4 (arts&music)" . "https://rthk.hk/live4.m3u")
     ("BBC World Service East Asia" . "http://stream.live.vc.bbcmedia.co.uk/bbc_world_service")
     ("Al Jazeera English" . "https://live-hls-web-aje.getaj.net/AJE/index.m3u8")
     ("Yle Radio 1" . "http://yleradiolive.akamaized.net/hls/live/2027672/in-YleRadio1/master.m3u8")
     ("AnimeTracks.com" . "http://69.162.124.26:8004/listen.pls")
     ("Classic FM" . "https://media-ice.musicradio.com/ClassicFM")
     ("LINN Classical" . "http://radio.linn.co.uk:8004/autodj")
     ("SomaFM - Deep Space One" . "https://somafm.com/deepspaceone.pls")
     ("SomaFM - The Dark Zone" . "https://somafm.com/darkzone.pls")
     ("SomaFM - Vaporwaves" . "https://somafm.com/vaporwaves.pls")))
  (empv-radio-log-file (expand-file-name "~/Music/logged-radio-songs.org"))
  :hook
  (empv-init . empv-override-quit-key)
  :bind-keymap
  ("C-c m" . empv-map)
  :bind
  ("<AudioPrev>" . #'empv-playlist-prev)
  ("<AudioNext>" . #'empv-playlist-next)
  ("<AudioPlay>" . #'empv-toggle)
  (:map empv-map
        ("l" . #'empv-playlist-select)
        ("p" . #'empv-playlist-prev)
        ("L" . #'empv-log-current-radio-song-name)
        ("N" . nil)
        ("P" . #'empv-youtube-playlist)
        ("S" . #'empv-playlist-save-to-file)
        ;; not #'working for org-mode url, seems to stripped the "http" part away
        (";" . #'empv-play-thing-at-point)
        ("," . #'empv-set-volume)
        ("." . #'empv-seek)
        ("+" . #'empv-seek-forward)
        ("-" . #'empv-seek-backward))
  (:repeat-map my/empv-seek-repeat-map
               ("+" . #'empv-seek-forward)
               ("-" . #'empv-seek-backward))
  (:repeat-map my/empv-playlist-repeat-map
               ("n" . #'empv-playlist-next)
               ("p" . #'empv-playlist-prev)
               ("s" . #'empv-playlist-shuffle))
  :config
  
  (defun empv-seek-forward ()
    (interactive)
    (empv-seek "15" '("relative")))
  (defun empv-seek-backward ()
    (interactive)
    (empv-seek "-5" '("relative")))
  
  (add-to-list 'empv-mpv-args "--ytdl-format=bestvideo[height<=?720]+bestaudio/best[ext=mp4]/best")
  ;; TODO
  (with-eval-after-load 'embark (empv-embark-initialize-extra-actions)))

;;; project.el
(use-package project
  :ensure nil
  :custom
  (project-list-file (expand-file-name "projects" user-cache-directory))
  :bind
  (:map project-prefix-map
        ("c" . #'project-recompile)
        ("C-c" . #'project-compile)))

;; Prevent `edebug' default bindings from interfering.
(setq edebug-inhibit-emacs-lisp-mode-bindings t)
(use-package activities
  :custom
  (activities-kill-buffers t)
  :init
  (activities-mode)
  (activities-tabs-mode)
  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)
   ("C-x C-a r" . activities-rename)
   ("C-x C-a C-<backspace>" . activities-discard)))

(use-package flymake
  :ensure nil)

;;; LSP - Eglot
(use-package eglot
  :ensure nil
  :custom
  ;; NOTE: doom disabled it
  (eglot-events-buffer-size 0)
  (eglot-sync-connect 1)
  (eglot-autoshutdown t)
  :bind
  (:map eglot-mode-map
        ("M-z M-;" . #'eglot-code-actions)
        ("M-z M-r" . #'eglot-rename)
        ("M-z M-d" . #'eglot-find-declaration)
        ("M-z M-t" . #'eglot-find-typeDefinition)
        ("M-z M-i" . #'eglot-find-implementation)
        ("M-z M-q" . #'eglot-shutdown)
        ("M-z C-M-q" . #'eglot-shutdown-all)))

(use-package consult-eglot
  :after eglot
  :bind
  (:map eglot-mode-map
        ([remap xref-find-apropos] . #'consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (consult-eglot embark)
  :config (consult-eglot-embark-mode 1))

(use-package eglot-tempel
  :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))

;;; dape
;; HOLD 
;; (use-package dape)

;;; Formatter
(use-package apheleia
  :bind
  ("C-c ;" . apheleia-format-buffer)
  ;; :init
  ;; ;; enabling auto format for all buffer
  ;; (apheleia-global-mode t)
  )

;;; Langs

;;; treesit
(use-package treesit
  :ensure nil
  :preface
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))
  :custom
  (treesit-extra-load-path '("~/src/tree-sitter-module-v2.5/")))

;;; elisp
(use-package ielm
  :ensure nil
  :custom
  (ielm-history-file-name (expand-file-name "ielm-history.eld" user-cache-directory))
  :bind
  ("C-c o r i" . ielm))

;;; Bash
(use-package sh-script
  :ensure nil
  :hook
  (sh-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))

;;; fish
(use-package fish-mode)

;;; Rust
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :hook
  (rust-mode . indent-bars-mode))

(use-package rustic
  :after rust-mode
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-lsp-client 'eglot))

;; just
(use-package just-mode)
(use-package justl)


;;; Ruby
(use-package ruby-mode
  :ensure nil
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :hook
  ;; FIXME Disabling popupinfo until this resolves
  ;; https://github.com/dgutov/robe/issues/144
  (ruby-base-mode . (lambda () (setq-local corfu-popupinfo-delay '(nil . 0.2))))
  (ruby-base-mode . flymake-mode)
  (ruby-base-mode . indent-bars-mode)
  (ruby-base-mode . subword-mode)
  :config
  (with-eval-after-load 'apheleia
    (setf (alist-get 'ruby-mode apheleia-mode-alist)
          '(rubocop))
    (setf (alist-get 'ruby-ts-mode apheleia-mode-alist)
          '(rubocop)))
  (add-to-list 'eglot-server-programs '(ruby-base-mode "ruby-lsp")))

(use-package ruby-end
  :ensure
  (:host github :repo "Kolmas225/ruby-end.el" :files ("*.el"))
  :custom
  (ruby-end-insert-newline nil))

(use-package robe
  :bind
  (:map robe-mode-map
        ("C-c M-j" . #'robe-start))
  :init
  (global-robe-mode))

(use-package inf-ruby
  :custom
  (inf-ruby-default-implementation "pry")
  :hook
  (inf-ruby-mode . puni-mode)
  :bind
  ("C-c o r r" . inf-ruby)
  (:map inf-ruby-minor-mode-map
        ("C-c M-s" . #'inf-ruby-console-auto)
        ("C-c C-c" . #'ruby-send-buffer)
        ("C-c M-q" . #'my/ruby-reset))
  :config
  (defun my/inf-ruby-robe-start (orig &rest args)
    "Start robe-session if in a ruby buffer and robe isn't started"
    (if (and (derived-mode-p 'ruby-base-mode)
             (not robe-running))
        (progn
          (apply orig args)
          (robe-start)
          (ruby-switch-to-last-ruby-buffer))
      (apply orig args)))
  (advice-add #'inf-ruby :around #'my/inf-ruby-robe-start)

  (defun my/ruby-reset ()
    "Reset repl environment on `pry'"
    (interactive)
    (process-send-string (inf-ruby-proc) "reset\r")))

(use-package rake
  :custom
  (rake-cache-file (expand-file-name "rake.cache" user-cache-directory))
  (rake-completion-system 'default))

(use-package rspec-mode
  :custom
  (rspec-use-rvm (executable-find "rvm"))
  (rspec-use-spring-when-possible nil))

;;; Crystal
(use-package crystal-mode
  :mode "\\.cr\\'"
  :interpreter "crystal")

;;; Elixir
(use-package elixir-ts-mode
  :ensure nil
  :mode ("\\.ex\\'" . elixir-ts-mode)
  :bind
  (:map elixir-ts-mode-map
        ("M-RET" . #'my/insert-elixir-pipe-operator))
  :config
  ;; modified from https://www.bounga.org/emacs/2020/04/22/easily-insert-elixir-pipe-operator-in-emacs/
  (defun my/insert-elixir-pipe-operator ()
    "Insert a newline and the |> operator"
    (interactive)
    (end-of-line)
    (unless (save-excursion
              (beginning-of-line)
              (looking-at-p "[[:space:]]*$"))
      (newline-and-indent))
    (insert "|> ")))

(use-package inf-elixir
  :after elixir-ts-mode
  :bind
  (:map elixir-ts-mode-map
        ("C-c C-s" . inf-elixir)
        ("C-c C-b" . inf-elixir-send-buffer)
        ("C-c C-r" . inf-elixir-send-region)
        ("C-x C-e" . inf-elixir-send-line)))

;;; Python
(use-package python
  :ensure nil
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :bind
  ("C-c o r p" . run-python)
  (:repeat-map my/python-indent-map
               ("<" . #'python-indent-shift-left)
               (">" . #'python-indent-shift-right))
  :hook
  (python-base-mode . (lambda () (setq-local tab-always-indent t)))
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  ;; :config
  ;; (with-eval-after-load 'apheleia
  ;;   (setf (alist-get 'python-ts-mode apheleia-mode-alist)
  ;;         '(ruff)))
  )

;;; clojure
(use-package clojure-ts-mode
  :custom
  (clojure-ts-toplevel-inside-comment-form t)
  :hook
  (clojure-ts-mode . cider-mode)
  :mode
  ("\\.clj\\'" . clojure-ts-mode))

(use-package cider
  :custom
  (cider-jack-in-default 'babashka)
  (cider-repl-display-help-banner nil)
  (org-babel-clojure-backend 'cider))

;;; Janet
(use-package janet-ts-mode
  :ensure (:host github :repo "sogaiu/janet-ts-mode" :files ("*.el"))
  :mode "\\.janet\\'"
  :interpreter "janet"
  :init
  (add-to-list 'treesit-language-source-alist
               '(janet-simple . ("https://github.com/sogaiu/tree-sitter-janet-simple"))))

(use-package ajrepl
  :ensure (:host github
                 :repo "sogaiu/ajrepl"
                 :files (:defaults ("ajrepl/"
                                    "ajrepl/*")))
  :after janet-ts-mode
  :bind
  (:map janet-ts-mode-map
        ("C-c C-s" . #'ajrepl))
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajrepl-interaction-mode))

;;; lua
(use-package lua-ts-mode
  :ensure nil
  :mode ("\\.lua\\'" . lua-ts-mode)
  :hook (lua-ts-mode . eglot-ensure)
  :init
  (add-to-list 'eglot-server-programs '(lua-ts-mode . ("stylua" "-"))))

;;; json
(use-package json
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode)))

;;; yaml
(use-package yaml-mode
  :mode ("\\.y[a]?ml\\'" . yaml-mode))

;;; web-mode
(use-package web-mode
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode))

(use-package gptel
  :preface
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(gemma3:4b))
  :custom
  (gptel-model 'gemma3:4b)
  (gptel-backend (gptel-get-backend "Ollama"))
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  :bind
  ("C-c g g" . #'gptel-menu)
  ("C-c g b" . #'gptel)
  ("C-c g s" . #'gptel-send)
  ("C-c g r" . #'gptel-rewrite)
  :config
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"))

(provide 'init)
;;; init.el ends here
