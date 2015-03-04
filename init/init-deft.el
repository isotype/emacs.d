;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 21-05-2014                                            ;;;
;;; Last-Updated: 11-02-2015                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-deft                                            ;;;
;;; Version:                                                       ;;;
;;; Description: Deft is a mode for managing notes                 ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'deft)
(setq deft-directory "~/Dev/.deft"
      deft-text-mode 'org-mode
      deft-extension "org"
      deft-auto-save-interval 10.0
      deft-use-filename-as-title t
      deft-time-format "| %d-%m-%Y %T")
(global-set-key (kbd "<f5>") 'deft)

(provide 'init-deft)
