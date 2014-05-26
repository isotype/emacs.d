;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 21-05-2014                                            ;;;
;;; Last-Updated: 25-05-2014                                       ;;;
;;;   By: Anton Strilchuk <anton@isoty.pe>                         ;;;
;;;                                                                ;;;
;;; Filename: init-deft                                            ;;;
;;; Version:                                                       ;;;
;;; Description: Deft is a mode for managing notes                 ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'deft)
(setq deft-text-mode 'org-mode)
(global-set-key (kbd "H-m") 'deft)
(global-set-key (kbd "H-n") 'deft-new-file-named)

(provide 'init-deft)
