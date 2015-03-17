;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 28-03-2014                                            ;;;
;;; Last-Updated: 16-03-2015                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-ido                                             ;;;
;;; Description: IDO, and IDO smex config                          ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;IDO Search
(require-package 'flx-ido)
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
(setq flx-ido-threshhold 1000)

(require-package 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; Use ido everywhere
(require-package 'ido-ubiquitous)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(setq ido-ubiquitous-use-new-completing-read 'webjump)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (if (and ido-use-virtual-buffers (fboundp 'ido-toggle-virtual-buffers))
       (ido-switch-buffer)
    (find-file (ido-completing-read "Open file: " recentf-list nil t))))

(global-set-key (kbd "C-c C-f") 'recentf-ido-find-file)
(define-key ctrl-apos (kbd "C-f") 'ido-find-file)

;;ibuffer
(require 'init-ibuffer)

;;SMEX M-x IDO
(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'smex)
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

;;SMEX Keys
(define-key ctrl-apos (kbd "C-c") 'smex)
(define-key ctrl-apos (kbd "M-x") 'smex)
(define-key ctrl-apos (kbd "c") 'smex-major-mode-commands)

;; Bookmark
(require 'bookmark)

;; Add Bookmark list to ido
(setq enable-recursive-minibuffers t)
(defun ido-goto-bookmark (bookmark)
  (interactive
   (list (bookmark-completing-read "Jump to bookmark" bookmark-current-bookmark)))
  (unless bookmark
    (error "No bookmark specified"))
  (let ((filename (bookmark-get-filename bookmark)))
    (if (file-directory-p filename)
        (progn
          (ido-set-current-directory filename)
          (setq ido-text ""))
      (progn
        (ido-set-current-directory (file-name-directory filename))))
    (setq ido-exit 'refresh
          ido-text-init ido-text
          ido-rotate-temp t)
    (exit-minibuffer)))

(define-key ido-file-dir-completion-map [(control ?1)] 'ido-goto-bookmark)
(global-set-key (kbd "C-M-<return>") 'bookmark-set)


(provide 'init-ido)
