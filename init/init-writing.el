;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 03-05-2014                                            ;;;
;;; Last-Updated: 25-05-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-writing                                         ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Speelin Chek
(require-package 'ispell)
(after-load "ispell"
  (progn
    (setq ispell-dictionary "british"
          ispell-silently-savep t)))

(add-to-list 'load-path (expand-file-name "writegood-mode" user-git-libraries))
(require-package 'writegood-mode)
;; Writegood mode
(global-set-key "\C-cg" 'writegood-mode)

(global-set-key (kbd "C-c q") 'refill-mode)

(provide 'init-writing)
