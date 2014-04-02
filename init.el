;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 23-03-2014                                            ;;;
;;; Last-Updated: 01-04-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init                                                 ;;;
;;; Description: Emacs Init                                        ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'init-benchmarking) ;; Measure startup time

(defconst *is-a-mac* (eq system-type 'darwin))

;; ---------------- ;;
;; Bootstrap Config ;;
;; ---------------- ;;
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

(setq
 user-mail-address "anton@isoty.pe"
 user-full-name "Anton Strilchuk")

(require 'init-keys) ; Keys and Passwords, do not include in public git
(require 'init-frame-hooks)
(require 'init-xterm)
(require 'fold-dwim)
;;Appearance Setup
(require 'init-theme)
(require 'init-appearance)
;; No annoy emacs beep
(setq ring-bell-function #'ignore)

;;Customizations
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

;;System setups
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

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

;;Path
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;Global Line Numbers
(global-linum-mode t)

;;Show keystrokes
(setq echo-keystrokes 0.02)

;;Don't show startup screen
(setq inhibit-startup-screen t)

;;Search Modes
(require 'init-search)
(require 'init-ido)

;; wgrep needed for init-edit-utils
(require 'wgrep)
(require 'init-edit-utils)

;;Drag Stuff is a minor mode for Emacs that makes
;;it possible to drag stuff (words, region, lines) around in Emacs
(require 'drag-stuff)
(drag-stuff-mode t)

;;Project management
(require 'ack-and-a-half)
(require 'projectile)
(projectile-global-mode)

;;Sauron events tracking
;; (require 'sauron)
;; (setq sauron-separate-frame nil
;;       sauron-hide-mode-line t
;;       sauron-sticky-frame t ;Sauron window appear on every (virtual) desktop
;;       sauron-dbus-cookie t
;;       sauron-max-line-length 120)
;; (sauron-start)

;; (global-set-key (kbd "C-c s") 'sauron-toggle-hide-show)
;; (global-set-key (kbd "C-c t") 'sauron-clear)

;;Load GTAGS for getting tags from source files
(setq load-path (cons "/usr/local/Cellar/global/6.2.9/share/gtags/" load-path))
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

;;Auto Complete
(require 'auto-complete)
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (ac-config-default)
  (define-key ac-completing-map (kbd "ESC") 'ac-stop)
  (global-auto-complete-mode t)
  (setq ac-delay 0.1
	ac-auto-show-menu 0.3
	ac-auto-start 1
	ac-quick-help-delay 1.0
	ac-quick-help-prefer-pos-tip t
	ac-ignore-case nil
	ac-candidate-menu-min 2
	ac-use-quick-help t
	ac-limit 10
	ac-disable-faces nil)
  (setq ac-sources-yasnippet t)
  (ac-flyspell-workaround))

;; Completion words longer than 4 characters
(custom-set-variables
  '(ac-ispell-requires 4))

(after-load "auto-complete"
      (ac-ispell-setup))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'org-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mu4e-compose-mode-hook 'ac-ispell-ac-setup)
(add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)

;;Yasnippet
(require 'yasnippet)
(yas/global-mode t)

;;Key modifiers
(setq ns-option-modifier 'meta)
(setq ns-command-modifier 'super)
(setq ns-right-command-modifier 'hyper)
(setq ns-right-option-modifier 'alt)
(setq ns-right-control-modifier 'nil)

;;Git
(require 'init-git)

;;Dash
(require 'init-dash)

;;Social Networking
(require 'init-social)

;;Rainbow Delimiter
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)

;;Rainbow CSS
(rainbow-mode t)

;;Multiple Cursors
(require 'multiple-cursors)
(multiple-cursors-mode t)

;;Buffer Backups (files in ~/.emacs.d/backup)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;;Flyspell spell-mode
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;SLIME
(load (expand-file-name "/Users/anton/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'slime-autoloads)
;; Replace "sbcl" with the path to your implementation
;;(setq slime-lisp-implementations "sbcl")
(slime-setup '(slime-fancy))
;;AC-Slime
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(setq slime-threads-update-interval 0.5)

(require 'terminal-notifier)
(require 'itail)

;;Load init-javascript
(require 'init-javascript)

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
(add-to-list 'ac-modes 'mu4e-compose-mode)

;;Markdown mode
(require 'init-markdown)

;;LATEX
(require 'init-latex)

;;Sprint.ly private stuff
(load "sprintly-mode-setup")

;;Ruby Setup
(load "erb-setup")

;;The Big Giant Org
(load "org-custom")
(require 'init-org-page)

;;Custom Keybindings
(load "keybindings")

;;New packages(Added: Jan 17)
;;Word Count Mode
;;https://github.com/bnbeckwith/wc-mode
(require 'wc-mode)
;; Suggested setting
(global-set-key "\C-cw" 'wc-mode)

;;Rebox2 Test
(setq rebox-style-loop '(23 223 26))
(require 'rebox2)
(global-set-key [(meta q)] 'rebox-dwim)
(global-set-key [(shift meta q)] 'rebox-cycle)

;;Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-extra-auto-pairs 
      '(("erb"  . (("open" "close")))
        ("php"  . (("open" "close")
                   ("open" "close")))
	))
(defun web-mode-hook ()
  (add-hook 'local-write-file-hooks
            (lambda ()
	      (delete-trailing-whitespace)
	      nil)))

(add-hook 'web-mode-hook  'web-mode-hook)

;;Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;Set/Refresh Font
;;Font
;;(setq default-frame-alist '(font . "Fira Mono-16"))
(set-face-attribute 'default nil
                    :family "Fira Mono"
		    :height 145
		    :weight 'bold)

;;----------------------------------------------------------------------------

;; variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

;;; Key Frequency Usage Statistics
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
