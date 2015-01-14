;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 24-03-2014                                            ;;;
;;; Last-Updated: 14-01-2015                                       ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Filename: init-social                                          ;;;
;;; Description: Setup for social networks                         ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-social)

;; FIXME:
;; (require-package 'skype)
;; (when (require 'skype)
;;   (setq skype--my-user-handle "antonstrilchuk"))

;;; TWITTER
;; Configurations for twittering-mode
;; Package from MELPA
(require-package 'twittering-mode)
(setq
 twittering-use-master-password t ; write to ~/.twittering-mode.gpg
 twittering-icon-mode t
 twittering-use-icon-storage t ; store icons to allow quicker loading
 twittering-enable-unread-status-notifier t ; unread status in mode line
)

;; Twitter Bit.ly Link Shortening
(setq twittering-tinyurl-service 'bit.ly)

;;; Spell Checking
(add-hook 'twittering-edit-mode-hook
          (lambda ()
            (ispell-minor-mode)
            (flyspell-mode)))
