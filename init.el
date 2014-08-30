;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 16-06-2014                                              ;;
;; Last-Updated: 30-08-2014                                         ;;
;;  Update #: 52                                                    ;;
;;   By: Anton Strilchuk <anton@ilyfa.cc>                           ;;
;;                                                                  ;;
;; Filename: init                                                   ;;
;; Version: 0.0.0.0.0.1                                             ;;
;; Description: ype'S emacs conf                                    ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(let ((minver 23))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-x-toolkit* (eq window-system 'x))
(defconst *is-ns-toolkit* (eq window-system 'ns))

;; Test to check if we are using XQuartz, to set correct .emacs.d
(when *is-x-toolkit*
  (setq user-emacs-directory "/opt/xwindows/emacs24/share/.emacs.d/"))

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init-tools" user-emacs-directory))

;; Add function for requiring git submodules
(require 'init-git-submodules)

;; Measure startup time
(require 'init-benchmarking)

;; ---------------- ;;
;; Bootstrap Config ;;
;; ---------------- ;;
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-git-packages)
(require 'init-exec-path)

;; wgrep needed for init-edit-utils
(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)
(require-package 'itail)

(setq temporary-file-directory (expand-file-name "backup" user-emacs-directory))
;;Buffer Backups (files in ~/.emacs.d/backup)
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq user-mail-address "ype@env.sh"
      user-full-name "Anton Strilchuk")

(require 'init-keys) ; Keys and Passwords, do not include in public git
(require 'init-frame-hooks)
(require 'init-xterm)
(require-package 'fold-dwim)
;;Appearance Setup
(require 'init-theme)
(require 'init-gui-frames)
(require 'init-appearance)
(unless *is-x-toolkit*
  (require 'init-font))

;;Customizations
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

;;Search Modes
(require 'init-search)
(require 'init-ido)
(require 'init-auto-complete)
(require 'init-windows)
(require 'init-sessions)


(require 'init-edit-utils)
(require 'init-helpers)
(require 'init-paredit)
(require 'init-flycheck)

;;Project management
(require-package 'ack-and-a-half)
(require 'init-proj-manage)

;;Git
(require 'init-git)

;; Paradox Package Rankings from GitHub
(require 'init-paradox-github)

;;Dash
(require 'init-dash)

;; Finances
(require 'init-ledger)

;;Social Networking
(require 'init-social)


;; Code Modes
(require 'init-javascript)
(require 'init-lisp)
(require 'init-slime)
(require 'init-common-lisp)
(require 'init-clojure)
(require 'init-literate-clojure)
(require 'init-python)
(require 'init-ruby)
(require 'init-oascript)
;;Custom Functions
(require 'init-random-defuns)

;;Jump to Page
(require 'init-webjump)

;;Tabs
;; Currently Disabled due to display lag issues
(require 'init-tabbar)

;;Auto Header
(require 'init-headers)

;;Clean up modeline
;;(load "clean-modeline")

(require-git-submodule 'terminal-notifier t)

;;Markdown mode
(require 'init-markdown)

;; Org-Sync
(require-git-submodule 'org-sync)
(require 'os)
;;The Big Giant Org
(require 'init-org)

;;; Quick create blog post
;; set posts directory
;; (setq ype/posts-directory "~/Dropbox/ype/isotype/content/posts")
;; (require 'init-blog-post)

;; Org Custom Macros
(require 'init-org-macros)

;; R in Emacs
(require 'init-ess)

;;LATEX
(require 'init-latex)

;;Custom Keybindings
(require 'init-keybindings)

;;MU4E
(require 'init-contacts)
(require 'init-mu4e)

;;IRC
(require 'init-irc)


;;,---------------------
;;| MISC
;;| miscellaneous stuff
;;`---------------------

;; Writing
(require 'init-deft)
(require 'init-writing)

;; Web
(require 'init-web)

;; Time Tracking
(require 'init-wakatime)

;; Health
(require 'init-rsi)

;; News and Reading
(require 'init-feeds)
(require 'init-spritz)

;; Slack
(require 'init-slack)

;; OSX Browse
;;(require 'init-browse)

;;,--------------------------------------------------
;;| MISC: OSX Printing
;;| From: http://www.emacswiki.org/emacs/MacPrintMode
;;`--------------------------------------------------
(add-to-list 'load-path (expand-file-name "misc" user-emacs-directory))
(require 'mac-print-mode)

(require-package 'gnuplot)
(when *is-a-mac*
  (require-package 'osx-location))
(require-package 'regex-tool)

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

;;Cleans up mode line clutter
;;(require 'init-mode-line-declutter)

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

;; Clock in default task (Daily Dose)
;; Jump: [[file:init/init-org.el::%3B%3B|%20DEFAULT%20TASK%20IDs][Default task ID function]]
;;(ype/clock-in-default-task-as-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
