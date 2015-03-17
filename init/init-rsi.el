;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <ype@env.sh>                             ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 02-06-2014                                              ;;
;;; Last-Updated: 16-03-2015                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-rsi                                               ;;
;; Version:                                                         ;;
;; Description: type-break mode config                              ;;
;; Depends on: init-edit-utils (m2s function)                       ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require-package 'type-break)

(setq type-break-interval 1200 ;; 20 Mins
      type-break-good-rest-interval 300 ;; 5 Mins
      type-break-keystroke-threshold '(1 . 8000)
      type-break-time-warning-intervals '(1 . 300)
      type-break-demo-functions '(type-break-demo-boring)
      type-break-mode-line-message-mode t
      type-break-terse-messages t)

(provide 'init-rsi)
