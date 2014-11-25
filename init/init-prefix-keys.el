;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Anton Strilchuk <anton@env.sh>                           ;;
;; URL: http://ype.env.sh                                           ;;
;; Created: 13-11-2014                                              ;;
;;; Last-Updated: 17-11-2014                                       ;;;
;;;  Update #: 13                                                  ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;                                                                  ;;
;; Filename: init-prefix-keys                                       ;;
;; Version:                                                         ;;
;; Description:                                                     ;;
;;                                                                  ;;
;; Package Requires: ()                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'ctrl-apos)
(global-set-key (kbd "C-\'") 'ctrl-apos)

(define-prefix-command 'escape-me)
(global-set-key (kbd "<escape>") 'escape-me)
(define-key escape-me (kbd "<escape>") 'keyboard-escape-quit)
(define-key escape-me (kbd "RET") 'save-buffer)
(define-key escape-me (kbd "DEL") 'delete-other-window)
(define-key escape-me (kbd "1") 'smex)
(define-key escape-me (kbd "2") 'ido-find-file)
(define-key escape-me (kbd "3") 'ido-find-file-other-window)
(define-key escape-me (kbd "4") 'recentf-open-files)


(provide 'init-prefix-keys)
