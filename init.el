;; -------------------------------------------------------------------- ;;
;; ; init.el ---                                                        ;;
;;                                                                      ;;
;;  Filename: init.el                                                   ;;
;;  Description:                                                        ;;
;;  Author: y_pe                                                        ;;
;;  Maintainer:                                                         ;;
;;  Created: Tue Feb 11 16:53:36 2014 (+0000)                           ;;
;;  Version:                                                            ;;
;;  Package-Requires: ()                                                ;;
;;  Last-Updated: Sat Mar  8 18:35:47 2014 (+0000)
;;            By: anton
;;      Update #: 131                                                    ;;
;;  URL: isoty.pe                                                       ;;
;;  Doc URL: built-in                                                   ;;
;;  Keywords: dotemacs, init, custom                                    ;;
;;  Compatibility:                                                      ;;
;;  See various modes for compatibility                                 ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                      ;;
;; ; Commentary:                                                        ;;
;;   emacs customizations and defuns                                    ;;
;;                                                                      ;;
;;                                                                      ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                      ;;
;; ; Change Log:                                                        ;;
;;   - Added file header                                                ;;
;;   - Change tabbar keybindings                                        ;;
;;   - Added Multi-term, and multi-term-setup.el                        ;;
;;                                                                      ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                      ;;
;;  This program is free software; you can redistribute it and/or       ;;
;;  modify it under the terms of the GNU General Public License as      ;;
;;  published by the Free Software Foundation; either version 3, or     ;;
;;  (at your option) any later version.                                 ;;
;;                                                                      ;;
;;  This program is distributed in the hope that it will be useful,     ;;
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of      ;;
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   ;;
;;  General Public License for more details.                            ;;
;;                                                                      ;;
;;  You should have received a copy of the GNU General Public License   ;;
;;  along with this program; see the file COPYING.  If not, write to    ;;
;;  the Free Software Foundation, Inc., 51 Franklin Street, Fifth       ;;
;;  Floor, Boston, MA 02110-1301, USA.                                  ;;
;;                                                                      ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                      ;;
;; ; Code:                                                              ;;
;; -------------------------------------------------------------------- ;;
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

(require 'init-frame-hooks)
(require 'init-xterm)

;;Appearance Setup
(require 'init-appearance)

;;No annoy emacs beep
(setq ring-bell-function #'ignore)

;;Theme
(load-theme 'solarized-dark t)

;;Customizations
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

;;Powerline instead of modeline
(require 'powerline)
(powerline-default-theme)
(setq powerline-color1 "#073642")
(setq powerline-color2 "#002B36")

(set-face-attribute 'mode-line nil
                    :foreground "#fdf6e3"
                    :background "#2aa198"
                    :box nil
		    :inverse-video nil)
(set-face-attribute 'mode-line-inactive nil
		    :foreground "#586e75"
                    :background "#002b36"
                    :box nil)

;;Wrap Text
(global-visual-line-mode 1)

;;Highlight cursor line
(global-hl-line-mode 1)

;;System setups
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;;Deletes selected text when typing
(delete-selection-mode t)
(transient-mark-mode t)

;;Column numbers in bottom menu
(column-number-mode 1)

;;Keep a list of recently opened files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

;;Winner Mode
(winner-mode 1)

;;Removes annoying bar at top
(tool-bar-mode -1)

;;Remove Scroll bar
(scroll-bar-mode -1)

;;Auto-insert/close bracket pairs
(electric-pair-mode 1)

;;Highlight matching parens
(show-paren-mode 1)

;;Hightlight entire bracket expression
(setq show-paren-style 'expression)

;;Show current time
(display-time-mode 1)
(setq display-time-day-and-date t)
(setq display-time-use-mail-icon t)

;;Don't display battery life
(display-battery-mode -1)
(size-indication-mode -1)

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

;;IDO Search
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point nil
      ido-auto-merge-work-directories-length 0
      ido-use-virtual-buffers t)

;;flx-ido completion system
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq flx-ido-threshhold 10000)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(setq ido-ubiquitous-use-new-completing-read 'webjump)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (if (and ido-use-virtual-buffers (fboundp 'ido-toggle-virtual-buffers))
       (ido-switch-buffer)
     (find-file (ido-completing-read "Open file: " recentf-list nil t))))
(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

;;ibuffer
(require 'init-ibuffer)

;;SMEX M-x IDO
(require 'smex)
(smex-initialize)

;;Drag Stuff is a minor mode for Emacs that makes
;;it possible to drag stuff (words, region, lines) around in Emacs
(require 'drag-stuff)
(drag-stuff-mode t)

;;Project management
(require 'ack-and-a-half)
(require 'projectile)
(projectile-global-mode)

;;Sauron events tracking
(require 'sauron)
(setq sauron-separate-frame nil)
(setq sauron-hide-mode-line t)
;;Sauron window appear on every (virtual) desktop
(setq sauron-sticky-frame t)
(setq sauron-dbus-cookie t)

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

;;Sprint.ly private stuff
(load "sprintly-mode-setup")

;;Ruby Setup
(load "erb-setup")

;;The Big Giant Org
(load "org-custom")

;;Custom Keybindings
(load "keybindings")

;;New packages(Added: Jan 17)
;;Word Count Mode
;;https://github.com/bnbeckwith/wc-mode
(require 'wc-mode)
;; Suggested setting
(global-set-key "\C-cw" 'wc-mode)

;;Undo Tree
;;http://ergoemacs.org/emacs/emacs_best_redo_mode.html
(require 'undo-tree)
(global-undo-tree-mode 1)

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
;;(setq default-frame-alist '((font . "Fira Mono-14")))
(set-face-attribute 'default nil
                    :family "Fira Mono" :height 145 :weight 'bold)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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
