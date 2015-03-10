;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename: init-eshell                                          ;;;
;;; Created: 10-03-2015                                            ;;;
;;; Author: Anton Strilchuk <anton@env.sh>                         ;;;
;;; URL: http://ype.env.sh                                         ;;;
;;; Version:                                                       ;;;
;;; Last-Updated: 10-03-2015                                       ;;;
;;;  Update #: 6                                                   ;;;
;;;   By: Anton Strilchuk <anton@env.sh>                           ;;;
;;;                                                                ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eshell)
(require-package 'eshell-prompt-extras)
(after-load 'esh-opt
  (require 'virtualenvwrapper)
  (venv-initialize-eshell)
  (require 'eshell-prompt-extras)
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(global-set-key (kbd "<f2> <f2>") 'eshell)

(require 'helm-eshell)
(add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)))

(provide 'init-eshell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;init-eshell.el ends here
