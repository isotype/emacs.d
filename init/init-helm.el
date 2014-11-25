;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-helm                                            ;;;
;;; Created: 17-11-2014                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 17-11-2014                                       ;;;
;;;  Update #: 6                                                   ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'helm)
(require-package 'helm-swoop)
(require-package 'helm-flycheck)
(require-package 'helm-projectile)

(define-key ctrl-apos (kbd "t") 'helm-timers)
(define-key ctrl-apos (kbd "o") 'helm-multi-occur)
(after-load 'flycheck (define-key ctrl-apos (kbd "h f") 'helm-flycheck))

(after-load 'projectile
  (define-key ctrl-apos (kbd "p") 'helm-projectile)
  (define-key ctrl-apos (kbd "h p") 'helm-projectile-switch-project))

(provide 'init-helm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-helm.el ends here
