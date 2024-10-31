;;; early-init.el --- Emacs pre GUI configuration -*- lexical-binding: t; -*-

;; TODO make an overengineered function/macro to ensure dirs exist
;; define user dirs
(defconst user-init-directory (expand-file-name "init/" user-emacs-directory))
(defconst user-lisp-directory (expand-file-name "lisp/" user-emacs-directory))
(defconst user-cache-directory (expand-file-name "cache/" user-emacs-directory))
(defconst user-data-directory (expand-file-name "data/" user-emacs-directory))
(dolist (dir (list user-cache-directory user-data-directory))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29
;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" user-cache-directory)))

;; disable package.el to use elpaca
(setq package-enable-at-startup nil)

;; disable unused stuff
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ring-bell-function 'ignore
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      inhibit-startup-screen t
      ;; initial-buffer-choice t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t
      inhibit-compacting-font-caches t
      initial-major-mode 'fundamental-mode)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar

;; inhibit bidi, long line display improvement
;; ref: https://emacs-china.org/t/topic/25811/9
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; garbage collection optimisation
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar default-vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

;; MAYBE check how would it perform in multi-frame workflow?
(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ;doom-emacs' default
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist
                  vc-handled-backends default-vc-handled-backends)
	    (when (boundp 'after-focus-change-function)
	      (add-function :after after-focus-change-function #'+gc-after-focus-change))))

(provide 'early-init)
;;; early-init.el ends here
