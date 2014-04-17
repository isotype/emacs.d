;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 28-03-2014                                            ;;;
;;; Last-Updated: 15-04-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-edit-utils                                      ;;;
;;; Description: Tools to simplify life in emacs                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'unfill)
(require 'whole-line-or-region)

(when (fboundp 'electric-pair-mode)
  (setq-default electric-pair-mode 1))

(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 compilation-scroll-output t
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                eww-mode
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

(require 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

(transient-mark-mode t)

(global-set-key (kbd "RET") 'newline-and-indent)

(after-load 'subword
    (diminish 'subword-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;;Undo Tree
;;http://ergoemacs.org/emacs/emacs_best_redo_mode.html
(require 'undo-tree)
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

(require 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))

(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;; Winner Mode
;;Undo frame changes
(winner-mode 1)

;;Highlight matching parens
(setq show-paren-style 'expression)
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require 'expand-region)
(global-set-key (kbd "C-\-") 'er/expand-region)

(cua-selection-mode t)

;;; Ace Jump
;; C-c SPC => ace-jump-word-mode
;; C-u C-c SPC => ace-jump-char-mode
;; C-u C-u C-c SPC => ace-jump-line-mode
(require 'ace-jump-mode)
(global-set-key (kbd "C-\\") 'ace-jump-mode)
(global-set-key (kbd "C-~") 'ace-jump-word-mode)
(global-set-key (kbd "C-`") 'ace-jump-line-mode)

(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(defun duplicate-region (beg end)
  "Insert a copy of the current region after the region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (insert (buffer-substring beg end))))

(defun duplicate-line-or-region (prefix)
  "Duplicate either the current line or any current region."
  (interactive "*p")
  (whole-line-or-region-call-with-region 'duplicate-region prefix t))

(global-set-key (kbd "C-c C-p") 'duplicate-line-or-region)

;; Force Use Emacs Movement
(global-unset-key [M-left])
(global-unset-key [M-right])

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require 'move-text)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;;----------------------------------------------------------------------------
;; Fill column indicator
;;----------------------------------------------------------------------------
(when (eval-when-compile (> emacs-major-version 23))
  (require 'fill-column-indicator)
  (defun sanityinc/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

  (defun sanityinc/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (sanityinc/fci-enabled-p)
          (turn-on-fci-mode))))))

(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)

(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
   (do-left-margin (and (bolp) (> (current-left-margin) 0)))
   (loc (point-marker))
   ;; Don't expand an abbrev before point.
   (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
       (if do-left-margin (indent-to (current-left-margin)))
       (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(when (executable-find "ag")
  (require 'ag)
  (require 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "H-q") 'ag-project)
  (global-set-key (kbd "H-z") 'projectile-ag))

(require 'highlight-escape-sequences)
(hes-mode)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x"))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/idle-delay 1.0)
(guide-key-mode 1)
(diminish 'guide-key-mode)

(provide 'init-edit-utils)
