;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-helm                                            ;;;
;;; Created: 17-11-2014                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 15-12-2014                                       ;;;
;;;  Update #: 7                                                   ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-helm)
(require-package 'helm)
(require-package 'helm-swoop)
(require-package 'helm-flycheck)
(require-package 'helm-projectile)

(define-key ctrl-apos (kbd "h t") 'helm-timers)
(define-key ctrl-apos (kbd "o") 'helm-multi-occur)
(after-load 'flycheck (define-key ctrl-apos (kbd "h f") 'helm-flycheck))

(after-load 'projectile
  (define-key ctrl-apos (kbd "p") 'helm-projectile)
  (define-key ctrl-apos (kbd "h p") 'helm-projectile-switch-project))
