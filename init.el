;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 23-03-2014                                            ;;;
;;;                                                                ;;;
;;; Filename: init                                                 ;;;
;;; Description: Emacs Init                                        ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-x-toolkit* (eq window-system 'x))
(defconst *is-ns-toolkit* (eq window-system 'ns))

;; Git Packages
(defconst user-git-libraries (expand-file-name "from-git" user-emacs-directory))

;; Test to check if we are using XQuartz, to set correct .emacs.d
(when *is-x-toolkit*
  (setq user-emacs-directory "/opt/xwindows/emacs24/share/.emacs.d/"))

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

;; Measure startup time
(require 'init-benchmarking)
;; ---------------- ;;
;; Bootstrap Config ;;
;; ---------------- ;;
(require 'init-compat)
(require 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ----------------------------------------------------- ;;
;; Package Manager See ~Cask~ file for its configuration ;;
;; https://github.com/cask/cask                          ;;
;; ----------------------------------------------------- ;;
(require 'cask "~/.cask/cask.el")
(cask-initialize)
;;Keeps ~Cask~ file in sync with the packages
;;that you install/uninstall via ~M-x list-packages~
;;https://github.com/rdallasgray/pallet
(require 'pallet)

(require 'init-exec-path)

;;Key modifiers
(unless *is-x-toolkit*
  (setq ns-option-modifier 'meta)
  (setq ns-command-modifier 'super)
  (setq ns-right-command-modifier 'hyper)
  (setq ns-right-option-modifier 'alt)
  (setq ns-right-control-modifier 'nil))

;;Buffer Backups (files in ~/.emacs.d/backup)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq
 user-mail-address "anton@isoty.pe"
 user-full-name "Anton Strilchuk")

(require 'init-keys) ; Keys and Passwords, do not include in public git
(require 'init-frame-hooks)
(require 'init-xterm)
(require 'fold-dwim)
;;Appearance Setup
(require 'init-theme)
(require 'init-gui-frames)
(require 'init-appearance)
(unless *is-x-toolkit*
  (require 'init-font))

;; No annoy emacs beep
(setq ring-bell-function #'ignore)

;;Customizations
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

;;Show current time
(display-time-mode 1)
(setq display-time-day-and-date t)
(setq display-time-use-mail-icon t)

;;Delete to trash
(setq delete-by-moving-to-trash t)

;;Y for yes N for no
(defalias 'yes-or-no-p 'y-or-n-p)

;;Confirm Emacs Quit
(set-variable 'confirm-kill-emacs 'yes-or-no-p)

;;Root directory
(setq root-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))

;;Show keystrokes
(setq echo-keystrokes 0.02)

;;Search Modes
(require 'init-search)
(require 'init-ido)
(require 'init-auto-complete)

;; wgrep needed for init-edit-utils
(require 'wgrep)
(require 'init-edit-utils)
(require 'init-paredit)

;;Drag Stuff is a minor mode for Emacs that makes
;;it possible to drag stuff (words, region, lines) around in Emacs
(require 'drag-stuff)
(drag-stuff-mode t)

;;Project management
(require 'ack-and-a-half)
(require 'init-proj-manage)

;;Load GTAGS for getting tags from source files
(setq load-path (cons "/usr/local/Cellar/global/6.2.9/share/gtags/" load-path))
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

;;Git
(require 'init-git)

;; Paradox Package Rankings from GitHub
(require 'init-paradox-github)

;;Dash
(require 'init-dash)

;;Social Networking
(require 'init-social)

;;Flyspell spell-mode
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;Load init-javascript
(require 'init-javascript)

;;SBCL
(require 'init-lisp)
(require 'init-slime)
(require 'init-clojure)
(require 'init-common-lisp)

;;Literate Clojure
(require 'init-literate-clojure)

(require 'terminal-notifier)
(require 'itail)

;;Rainbow Blocks
(eval-after-load 'rainbow-blocks '(diminish 'rainbow-blocks-mode))
(add-hook 'lisp-mode-hook 'rainbow-blocks-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-blocks-mode)
(global-set-key (kbd "C-c C-d") 'rainbow-blocks-mode)

;;Custom Functions
(load "defuns")

;;Emacs Terminal
(load "multi-term-setup")

;;TMUX
(load "tmux_setup")

;;W3M
;;(load "y_pe-w3m")

;;Tabs
;; Currently Disabled due to display lag issues
(load "tabbar-custom")

;;Auto Header
(load "header_setup")

;;Clean up modeline
(load "clean-modeline")

;;MU4E
(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e")
(load "mu4e-custom")

;;Markdown mode
(require 'init-markdown)

;;; Quick create blog post
;; set posts directory
(setq ype/posts-directory "~/Dropbox/ype/isotype/content/posts")
(require 'init-blog-post)

;;Sprint.ly private stuff
(load "sprintly-mode-setup")

;;Ruby Setup
(load "erb-setup")

;;The Big Giant Org
(load "org-custom")

;; Org Custom Macros
(require 'init-org-macros)

;;LATEX
(require 'init-latex)

;;Custom Keybindings
(load "keybindings")

;; Rebox2 Test
(add-to-list 'load-path (expand-file-name "from-git/rebox2/" user-emacs-directory))

;;IRC
(require 'init-irc)
;; Writing
(require 'init-writing)
(require 'init-web)

;;----------------------------------------------------------------------------
;; Byte compile every .el file into a .elc file in the
;; given directory. Must go after all init-* require.
;; Source: http://ubuntuforums.org/archive/index.php/t-183638.html
;;----------------------------------------------------------------------------
(defun lw:byte-compile-directory(directory)
  (interactive
   (list
    (read-file-name "Lisp directory: "))))
;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Open bookmark menu on start
;;----------------------------------------------------------------------------
(bookmark-bmenu-list)

(require 'init-wakatime)
;;----------------------------------------------------------------------------
;; variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-locales)
(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
