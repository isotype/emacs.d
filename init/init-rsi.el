;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 02-06-2014                                              ;;
;; Last-Updated: 10-06-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;                                                                  ;;
;; Filename: init-rsi                                               ;;
;; Version:                                                         ;;
;; Description: type-break mode config                              ;;
;; Depends on: init-edit-utils (m2s function)                       ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq type-break-file-name "~/.emacs.d/.type-break"
      type-break-interval (m2s 20)
      type-break-good-rest-interval (m2s 5)
      type-break-keystroke-threshold '(1 . 8000)
      type-break-time-warning-intervals '((m2s 5) (m2s 10) (m2s 15))
      type-break-demo-functions '(type-break-demo-boring)
      type-break-mode-line-message-mode t
      type-break-terse-messages t)

(type-break-mode -1)

(provide 'init-rsi)
