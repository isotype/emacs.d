;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 28-03-2014                                            ;;;
;;; Last-Updated: 17-04-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-ido                                             ;;;
;;; Description: IDO, and IDO smex config                          ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(unless *is-x-toolkit*
  (require 'ido-ubiquitous)
  (ido-ubiquitous-mode 1)
  (setq ido-ubiquitous-use-new-completing-read 'webjump))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (if (and ido-use-virtual-buffers (fboundp 'ido-toggle-virtual-buffers))
       (ido-switch-buffer)
     (find-file (ido-completing-read "Open file: " recentf-list nil t))))
(global-set-key (kbd "C-c C-f") 'recentf-ido-find-file)

;;ibuffer
(require 'init-ibuffer)

;;SMEX M-x IDO
(when (eval-when-compile (>= emacs-major-version 24))
  (require 'smex)
  (global-set-key [remap execute-extended-command] 'smex))

(provide 'init-ido)
