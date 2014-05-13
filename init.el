;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 23-03-2014                                            ;;;
;;; Last-Updated: 13-05-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
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

;;Speelin Chekr
(require 'ispell)
(eval-after-load "ispell"
  (progn
    (setq ispell-dictionary "british"
          ispell-silently-savep t)))

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

;; ;;Auto Complete
;; (require 'auto-complete)
;; (when (require 'auto-complete-config nil 'noerror)
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
;;   (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
;;   (ac-config-default)
;;   (define-key ac-completing-map (kbd "ESC") 'ac-stop)
;;   (global-auto-complete-mode t)
;;   (setq ac-delay 0.1
;;  ac-auto-show-menu 0.3
;;  ac-auto-start 1
;;  ac-quick-help-delay 1.0
;;  ac-quick-help-prefer-pos-tip t
;;  ac-ignore-case nil
;;  ac-candidate-menu-min 2
;;  ac-use-quick-help t
;;  ac-limit 10
;;  ac-disable-faces nil)
;;   (setq ac-sources-yasnippet t)
;;   (ac-flyspell-workaround))

;; ;; Completion words longer than 4 characters
;; (custom-set-variables
;;   '(ac-ispell-requires 4))

;; (after-load "auto-complete"
;;       (ac-ispell-setup))

;; (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
;; (add-hook 'org-mode-hook 'ac-ispell-ac-setup)
;; (add-hook 'mu4e-compose-mode-hook 'ac-ispell-ac-setup)
;; (add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)

;; ;;Yasnippet
;; (require 'yasnippet)
;; (yas/global-mode t)

;;Key modifiers
(unless *is-x-toolkit*
  (setq ns-option-modifier 'meta)
  (setq ns-command-modifier 'super)
  (setq ns-right-command-modifier 'hyper)
  (setq ns-right-option-modifier 'alt)
  (setq ns-right-control-modifier 'nil))

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
(add-to-list 'ac-modes 'mu4e-compose-mode)

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

;;LATEX
(require 'init-latex)

;;Custom Keybindings
(load "keybindings")

;;New packages(Added: Jan 17)
;;Word Count Mode
;;https://github.com/bnbeckwith/wc-mode
(require 'wc-mode)
;; Suggested setting
(global-set-key "\C-cw" 'wc-mode)

;; Rebox2 Test

(add-to-list 'load-path (expand-file-name "from-git/rebox2/" user-emacs-directory))
(require 'rebox2)
(setq rebox-style-loop '(17 27 21))
(global-set-key [(meta q)] 'rebox-dwim)
(global-set-key [(shift meta q)] 'rebox-cycle)


(require 'init-nxml)
(require 'init-css)
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

;;IRC
(require 'init-irc)

;; Writing
(require 'init-writing)

;;plantuml
(setq org-plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory))
(setq plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory))
(require 'iimage)
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))

;; Rendering plantuml
(defun plantuml-render-buffer ()
  (interactive)
  (message "PLANTUML Start rendering")
  (shell-command (concat "java -jar ~/.emacs.d/plantuml.jar "
                         buffer-file-name))
  (message (concat "PLANTUML Rendered:  " (buffer-name))))

;; Image reloading
(defun reload-image-at-point ()
  (interactive)
  (message "reloading image at point in the current buffer...")
  (image-refresh (get-text-property (point) 'display)))

;; Image resizing and reloading
(defun resize-image-at-point ()
  (interactive)
  (message "resizing image at point in the current buffer123...")
  (let* ((image-spec (get-text-property (point) 'display))
         (file (cadr (member :file image-spec))))
    (message (concat "resizing image..." file))
    (shell-command (format "convert -resize %d %s %s "
                           (* (window-width (selected-window)) (frame-char-width))
                           file file))
    (reload-image-at-point)))

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
