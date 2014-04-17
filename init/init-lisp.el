;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 10-04-2014                                            ;;;
;;; Last-Updated: 17-04-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-lisp                                            ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(require 'lively)

(setq-default initial-scratch-message
              (concat ";; Happy hacking " (or user-login-name "") "!\n\n"))



;; Make C-x C-e run 'eval-region if the region is active

(defun sanityinc/eval-last-sexp-or-region (beg end prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "r\nP")
  (if (use-region-p)
      (eval-region beg end)
    (pp-eval-last-sexp prefix)))

(global-set-key (kbd "M-:") 'pp-eval-expression)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'sanityinc/eval-last-sexp-or-region))

(require 'ipretty)
(ipretty-mode 1)


;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

(defun my/emacs-lisp-module-name ()
  "Search the buffer for `provide' declaration."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "^(provide '" nil t)
      (symbol-name (symbol-at-point)))))

;; Credit to Chris Done for this one.
(defun my/try-complete-lisp-symbol-without-namespace (old)
  "Hippie expand \"try\" function which expands \"-foo\" to \"modname-foo\" in elisp."
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (when (string-prefix-p "-" he-search-string)
      (let ((mod-name (my/emacs-lisp-module-name)))
        (when mod-name
          (setq he-expand-list (list (concat mod-name he-search-string)))))))
  (when he-expand-list
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list nil)
    t))

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
  (add-to-list 'hippie-expand-try-functions-list 'my/try-complete-lisp-symbol-without-namespace t))

;; ----------------------------------------------------------------------------
;; Highlight current sexp
;; ----------------------------------------------------------------------------

(require 'hl-sexp)

;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
    (when turn-on
      (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))


;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(require 'rainbow-delimiters)
(require 'redshank)
(after-load 'redshank
  (diminish 'redshank-mode))


(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (rainbow-delimiters-mode t)
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (redshank-mode))

(defun sanityinc/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (elisp-slime-nav-mode t)
  (set-up-hippie-expand-for-elisp)
  (ac-emacs-lisp-mode-setup))

(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/elispy-modes))
  (add-hook hook 'sanityinc/emacs-lisp-setup))

(defun sanityinc/maybe-check-parens ()
  "Run `check-parens' if this is a lispy mode."
  (when (memq major-mode sanityinc/lispy-modes)
    (check-parens)))

(add-hook 'after-save-hook #'sanityinc/maybe-check-parens)

(require 'eldoc-eval)
(require 'eldoc-eval)

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

(require 'cl-lib-highlight)
(after-load 'lisp-mode
  (cl-lib-highlight-initialize))

;; ----------------------------------------------------------------------------
;; Other Stuff
;; ----------------------------------------------------------------------------
(require 'macrostep)

(after-load 'lisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(provide 'init-lisp)
