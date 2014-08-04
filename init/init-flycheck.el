;;; -*- mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode:nil; -*-  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Author: Anton Strilchuk <anton@isoty.pe>                       ;;;
;;; URL: http://isoty.pe                                           ;;;
;;; Created: 28-04-2014                                            ;;;
;; Last-Updated: 03-08-2014                                         ;;
;;   By: Anton Strilchuk <ype@env.sh>                               ;;
;;;                                                                ;;;
;;; Filename: init-flycheck                                        ;;;
;;; Version:                                                       ;;;
;;; Description:                                                   ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

(defun sanityinc/flycheck-errors-visible-p ()
  "Returns t if a window in the current frame is displaying \"*Flycheck errors*\"."
  (let (found-flycheck-errors-buf)
    (walk-window-tree (lambda (w)
                        (when (string-equal "*Flycheck errors*"
                          (buffer-name (window-buffer w)))
        (setq found-flycheck-errors-buf t))))
    (selected-frame)
    found-flycheck-errors-buf))

(defun sanityinc/flycheck-maybe-display-errors (errors)
  (unless (sanityinc/flycheck-errors-visible-p)
    (flycheck-display-error-messages errors)))

(setq flycheck-display-errors-function 'sanityinc/flycheck-maybe-display-errors)

(global-set-key (kbd "H-\.") 'flycheck-next-error)
(global-set-key (kbd "H-\,") 'flycheck-previous-error)

(provide 'init-flycheck)
